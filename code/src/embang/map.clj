(ns embang.map
  (:require [embang.colt.distributions :as dist])
  (:use [embang.state :exclude [initial-state]]
        [embang.runtime :only [sample observe]]
        embang.inference))

;;;; Maximum a Posteriori Estimation through Sampling

;; Uses MCTS to find maximum a posteriori estimate of
;; program trace.

(derive ::algorithm :embang.inference/algorithm)

;;;; Particle state

(def initial-state
  "initial state for MAP estimation"
  (into embang.state/initial-state
        {::bandits {}
         ::trace []}))

;;;; Bayesian updating, for randomized probability matching

(defprotocol bayesian-belief
  "Bayesian belief"
  (bb-update [belief evidence]
    "updates belief based on the evidence")
  (bb-sample [belief]
    "returns a random sample from the belief distribution")
  (bb-mode [belief]
    "returns the mode of the distribution"))

;;;; Mean reward belief, via Gamma distribution

(defn mean-reward-belief
  [shape rate]
  ;; Bayesian belief about the mean reward  (log-weight).
  ;; 
  ;; We use log-Gamma belief for rewards and impose 
  ;; Gamma prior on the rate of the reward distribution.
  (let [distribution (dist/gamma-distribution shape rate)]
    (reify bayesian-belief
      (bb-update [mr reward]
        (mean-reward-belief (+ shape 1.) (+ rate (Math/exp reward))))
      ;; Since  the mean reward distributions
      ;; are used for comparison only, the absolute value of
      ;; the shape of the reward distribution does not matter,
      ;; provided it is assumed to be the same for all arms.
      (bb-sample [mr]
        (Math/log (/ (dist/draw distribution))))
      (bb-mode [mr]
        (Math/log (/ rate (- shape 1.)))))))

(def initial-mean-reward-belief
  "initial belief about mean reward" 
  ;; Uninformative prior --- we know nothing about
  ;; the absolute values of rewards in general.
  (mean-reward-belief 1. Double/MIN_NORMAL))

;;;; Bandit

;; selects arms using randomized probability matching

;; Best arm is used both for randomized selection, 
;; scored by bb-sample, and for the final arm selection,
;; scored by bb-mode

(defn best-arm
  "select an arm with the best core"
  [arms bb-score best-score best-arm]
  (if-let [[[value belief :as arm] & arms] (seq arms)]
    (let [score (bb-score belief)]
      (if (>= score best-score)
        (recur arms bb-score score arm)
        (recur arms bb-score best-score best-arm)))
    best-arm))

(defn select-most-promising-arm
  "selects an arm by sampling from mean's belief,
  returns the value of an existing arm,
  or nil if a new value is to be drawn"
  [arms]
  ;; First, select an arm which belief will be used
  ;; for a bet of the new arm.
  (when-let [[_ belief] (best-arm arms bb-sample
                                  Double/NEGATIVE_INFINITY nil)]
    ;; Then, select an arm with the best bet; if the bet
    ;; of a new arm is the best, add a new arm.
    (when-let [[value _] (best-arm arms bb-sample
                                   (bb-sample belief) nil)]
      value)))

(defn select-maximum-a-posteriori-arm
  "returns the value of an arm with
  the highest mode of the belief,
  or nil if there are no arms"
  [arms]
  (when-let [[value _] (best-arm arms bb-mode
                                  Double/NEGATIVE_INFINITY nil)]
    value))

(defn update-arm 
  "updates the belief about arm in arms"
  [arms arm reward]
  (update-in arms [arm]
             (fnil bb-update initial-mean-reward-belief) reward))

;;;; MAP inference

;;; Random choice bandit

;; A bandit is in either default or frozen `mood.'  When
;; a bandit is in the default mood, the arm is selected by
;; sampling from the mean's belief. When the bandit is frozen,
;; the arm with the maximum probability mass is selected.

(defmulti select-arm :mood)

(defmethod select-arm :frozen [bandit]
  (select-maximum-a-posteriori-arm (:arms bandit)))

(defmethod select-arm :default [bandit]
  (select-most-promising-arm (:arms bandit)))

(defn update-bandit
  "updates bandit's belief"
  [bandit sample reward]
  (-> bandit
      (update-in [:arms] (fnil update-arm {}) sample reward)
      (update-in [:count] (fnil inc 0))))

(defn freeze-bandit
  "sets the bandit's mood to frozen"
  [bandit]
  (assoc bandit :mood :frozen))

(defn frozen?
  "true when the bandit is frozen"
  [bandit]
  (= (:mood bandit) :frozen))

;;; State transformations

(defn freeze
  "freeze bandits that satisfy the condition"
  [bandits can-be-frozen?]
  (reduce (fn [bandits id]
            (update-in bandits [id] freeze-bandit))
          bandits
          ;; find ids of all bandits that can be frozen
          (keep (fn [[id bandit]]
                  (when (can-be-frozen? bandit)
                    id))
                bandits)))

(defn backpropagate
  "back propagate reward to bandits"
  [state can-be-frozen?]
  (let [reward (get-log-weight state)]
    (loop [trace (state ::trace)
           bandits (state ::bandits)]
      (if (seq trace)
        (let [[[id sample past-reward] & trace] trace]
          (recur trace
                 (update-in bandits [id]
                            update-bandit sample (- reward past-reward))))
        (assoc initial-state
               ::bandits (freeze bandits can-be-frozen?))))))

(defn maximum-a-posteriori
  "given the state, returns a vector of
  maximum a posteriori samples"
  [state]
  (map second (state ::trace)))

;; Bandit id: different random choices should get different
;; ids, ideally structurally similar random choices should
;; get the same id, just like addresses in Random DB
(defn bandit-id [smp trace]
  "returns bandit id for the checkpoint,
  the id includes the complete trace prefix"
  [(:id smp) (count trace)])

(defmethod checkpoint [::algorithm embang.trap.sample] [algorithm smp]
  (let [state (:state smp)
        id (bandit-id smp (state ::trace))
        bandit ((state ::bandits) id)
        ;; Past reward is the reward collected by the particle
        ;; until the checkpoint. To make rewards collected by
        ;; arms commensurate past-reward is subtracted from
        ;; the final reward.
        past-reward (get-log-weight state)

        ;; select a value
        value (or (select-arm bandit) (sample (:dist smp)))

        ;; update the state
        state (-> state
                  ;; add the log-weight of the sample
                  (add-log-weight (observe (:dist smp) value))

                  ;; store the sampled value in the trace
                  (update-in [::trace] conj [id value past-reward]))]

    ;; Finally, continue the execution.
    #((:cont smp) value state)))

;; Freeze bandit nodes as a freeze condition
;; --- the simplest is the number of samples
;; per node --- has been reached.
;;
;; When the maximum total number of samples
;; is reached, freeze all nodes. After a trace
;; with all frozen nodes, print and return.

(defmethod infer :map [_ prog & {:keys [number-of-samples
                                        freeze-point
                                        output-format]
                                 :or {freeze-point 1}}]
  (loop [i 0
         end-state (:state (exec ::algorithm prog nil initial-state))]
    (let [begin-state (backpropagate end-state #(= (:count %) freeze-point))
          end-state (:state (exec ::algorithm prog nil begin-state))]
      (if (or (= i number-of-samples) (every? frozen? begin-state))
        (do
          (print-predicts end-state output-format)
          ;; return a vector of MAP sample choices
          (maximum-a-posteriori end-state))
        (recur (inc i) end-state)))))
