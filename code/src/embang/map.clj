(ns embang.map
  (:require [embang.colt.distributions :as dist])
  (:require [clojure.data.priority.map :refer []])
  (:use [embang.state :exclude [initial-state]]
        [embang.runtime :only [sample observe]]
        embang.inference))

;;;; Maximum a Posteriori Estimation through Sampling

;; Uses MCTS and best-first search to find maximum a
;; posteriori estimate of program trace.

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
    "returns a random sample from the belief distribution"))

;;;; Mean reward belief

(defn mean-reward-belief
  [shape rate]
  ;; Bayesian belief about the mean reward  (log-weight).
  (let [distribution ]
    (reify bayesian-belief
      (bb-update [mr reward]
        )
      (bb-sample [mr]
        ))))

;;;; Bandit

;; selects arms using randomized probability matching

(defn best-arm
  "select an arm with the best core"
  [arms best-score best-arm]
  (if-let [[[_ belief :as arm] & arms] (seq arms)]
    (let [score (bb-sample belief)]
      (if (>= score best-score)
        (recur arms score arm)
        (recur arms best-score best-arm)))
    best-arm))

(defn update-arm 
  "updates the belief about arm in arms,
  uses prior-belief for a new arm"
  [arms prior-belief arm reward]
  (update-in arms [arm]
             (fnil bb-update prior-belief) reward))

;;;; MAP inference

;;; Random choice bandit

(defn update-bandit
  "updates bandit's belief"
  [bandit sample reward]
  (-> bandit
      (update-in [:arms] (fnil update-arm {}) sample reward)
      (update-in [:count] (fnil inc 0))))

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
  [state]
  (let [reward (get-log-weight state)]
    (loop [trace (state ::trace)
           bandits (state ::bandits)]
      (if (seq trace)
        (let [[[id sample past-reward] & trace] trace]
          (recur trace
                 (update-in bandits [id]
                            update-bandit sample (- reward past-reward))))
        (assoc initial-state
               ::bandits bandits)))))

;;; Trace

;; The trace is a vector of tuples
;;   [bandit-id value past-reward]
;; where past reward is the reward accumulated 
;; before reaching this random choice.

;; Bandit id: different random choices should get different
;; ids, ideally structurally similar random choices should
;; get the same id, just like addresses in Random DB
(defn bandit-id [smp trace]
  "returns bandit id for the checkpoint"
  [(:id smp)  ; static identifier of the random choice
   (count     ; number of preceding draws from the same choice
     (filter (fn [[[smp-id]]]] (= smp-id (:id smp)))
             trace))])

(defmethod checkpoint [::algorithm embang.trap.sample] [algorithm smp]
  (let [state (:state smp)
        id (bandit-id smp (state ::trace))
        bandit ((state ::bandits) id)
        ;; Past reward is the reward collected by the particle
        ;; until the checkpoint. To make rewards collected by
        ;; arms commensurate, past-reward is subtracted from
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

;;; Best-first search, passive and functional
;;
;; A node is a delayed computation.  Nodes are inserted
;; into the open list ordered by the distance estimate.
;; When a node is removed from the open list, it is
;; forced, and then dispatched according to its type,
;; sample or result.
;;
;; On sample, the search continues.
;; On result, a sequence starting with the state
;; and followed by a lazy sequence of states of future
;; found estimates is returned.
;;
;; When the open list is empty, nil is returned.

(defn maximum-a-posteriori
  "returns a sequence of end states of 
  maximum a posteriori estimates"
  [prog begin-state]
  nil)

(defmethod infer :map [_ prog & {:keys [number-of-passes
                                        number-of-samples
                                        number-of-maps
                                        output-format]
                                 :or {number-of-passes 1
                                      number-of-maps 1}}]
  (dotimes [_ number-of-passes]
    (loop [isamples 0
           begin-state initial-state]

      ;; After each sample, the final rewards are
      ;; back-propagated to the bandits representing subsets
      ;; of random choices.
      (let [end-state (:state (exec ::algorithm prog nil begin-state))
            begin-state (backpropagate end-state)]
        (if-not (= isamples number-of-samples)
          (recur (inc i) begin-state)

          ;; The program graph is ready for MAP search.
          ;; Consume the sequence of end-states of MAP
          ;; estimates and print the predicts.
          (loop [imaps 0
                 end-states (maximum-a-posteriori prog begin-state)]
            (when-not (= imaps number-of-maps)
              (let [[end-state end-states] end-state]
                (when end-state  ; Otherwise, all paths were visited.
                  (print-predicts end-state output-format)
                  (recur (inc imaps) end-states))))))))))
