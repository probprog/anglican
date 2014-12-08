(ns embang.map
  (:require [embang.colt.distributions :as dist])
  (:use [embang.state :exclude [initial-state]]
        [embang.runtime :only [sample observe]]
        embang.inference))

;;;; Maximum a Posteriori Estimation Through Sampling

;; Uses MCTS to find maximum a posteriori estimate of
;; program trace

;; Unlike inference sampling algorithms, has two
;; execution modes:
;;  - the ::explore mode is used to discover 
;;    the most probable choices
;;  - the ::select mode is used to simulate
;;    a trace with the highest MAP estimate

(derive ::algorithm :embang.inference/algorithm)
(derive ::explore ::algorithm)
(derive ::select ::algorithm)

;;; Particle state

(def initial-state
  "initial state for MAP estimation"
  (into embang.state/initial-state
        {::bandits {}
         ::trace []}))

;;; Bayesian updating, for randomized probability matching

(defprotocol bayesian-belief
  "Bayesian belief"
  (bb-update [belief evidence]
    "updates belief based on the evidence")
  (bb-sample [belief]
    "returns a random sample from the belief distribution")
  (bb-mode [belief]
    "returns the mode of the distribution"))

;;; Mean reward belief, via Gamma distribution

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

;;; Bandit

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

(defn select-arm
  "returns the value of an existing arm,
  or nil if a new value to be drawn"
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

(defn select-map-arm
  "returns the value of an arm with
  the highest mode of the belief,
  o nil if there are no arms"
  [arms]
  (when-let [[value _] (best-arm arms bb-mode
                                  Double/NEGATIVE_INFINITY nil)]
    value))

(defn update-arm 
  "updates the belief about arm in arms"
  [arms arm reward]
  (update-in arms [arm]
             (fnil bb-update initial-mean-reward-belief) reward))

;;; MAP inference

;; State transformations

(defn backpropagate
  "back propagate reward to bandits"
  [state]
  (let [reward (get-log-weight state)]
    (loop [trace (::trace state)
           bandits (::bandits state)]
      (if (seq trace)
        (let [[[id sample shared-reward] & trace] trace]
          (recur trace
                 (-> bandits
                     (update-in [id :arms]
                                (fnil update-arm {})
                                sample (- reward shared-reward))
                     (update-in [id :count] (fnil inc 0)))))

        (assoc initial-state
               ::bandits bandits)))))

(defn maximum-a-posteriori
  "given the state, returns a vector of
  maximum a posteriori samples"
  [state]
  (map second (::trace state)))

(defmethod checkpoint [::algorithm embang.trap.sample] [algorithm smp]
  (let [state (:state smp)
        id [(:id smp) (count (::trace state))]
        arms (get-in (::bandits state) [id :arms])
        shared-reward (get-log-weight state)

        ;; select a value
        value (or ((case algorithm
                     ::explore select-arm     ; probability matching
                     ::select select-map-arm) ; greatest mode
                   arms)
                  ;; or sample a new value
                  (sample (:dist smp)))

        ;; update the state
        state (-> state
                  ;; add the log-weight of the sample
                  (add-log-weight (observe (:dist smp) value))

                  ;; store the sampled value in the trace
                  (update-in [::trace] conj [id value shared-reward]))]

    ;; Finally, continue the execution.
    #((:cont smp) value state)))

(defmethod infer :map [_ prog & {:keys [number-of-samples
                                        output-format]}]
  (loop [i 0
         state initial-state]
    (if-not (= i number-of-samples)
      (recur (inc i) (backpropagate
                       (:state (exec ::explore prog nil state))))
      (do
        (let [state (:state (exec ::select prog nil state))]
          (print-predicts state output-format)
          ;; return a vector of MAP sample choices
          (maximum-a-posteriori state))))))
