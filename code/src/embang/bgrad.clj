(ns embang.bgrad
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:require [clojure.data.priority-map
             :refer [priority-map-keyfn-by]])
  (:use [embang.state :exclude [initial-state]]
        embang.inference
        [embang.runtime :only [sample observe normal]]))

;;;;; Maximum a Posteriori Estimation through Sampling

(derive ::algorithm :embang.inference/algorithm)

;;;; Particle state

(def initial-state
  "initial state for MAP estimation"
  (into embang.state/initial-state
        {::bandits {}            ; multi-armed bandits
         ::trace []              ; random choices
         ::bandit-counts {}      ; counts of occurences of `sample's
         ::bandit-last-id nil})) ; last sample id

;;;; Bayesian updating, for randomized probability matching

(defprotocol bayesian-belief
  "Bayesian belief"
  (bb-update [belief evidence]
    "updates belief based on the evidence")
  (bb-sample [belief]
    "returns a random sample from the belief distribution")
  (bb-as-prior [belief]
    "returns a belief for use as a prior belief"))

;;;; Mean reward belief

(defn mean-reward-belief
  "returns reification of bayesian belief
  about the mean reward of an arm"
  [sum sum2 cnt]
  ;; Bayesian belief about the mean reward (log-weight).
  ;; Currently, the normal distribution with empirical
  ;; mean and variance is used.
  (let [dist (delay
               ;; The distribution object is lazy because
               ;; the parameters are updated many times,
               ;; but the object itself is only used when
               ;; a value is sampled.
               (let [mean (/ sum cnt)
                     sd (Math/sqrt (/ (- (/ sum2 cnt) (* mean mean))
                                      cnt))] ; Var(E(X)) = Var(X)/n
                 (normal mean sd)))]
    (reify bayesian-belief
      (bb-update [mr reward]
        (mean-reward-belief
          (+ sum reward) (+ sum2 (* reward reward)) (+ cnt 1.)))
      (bb-sample [mr] {:pre [(pos? cnt)]}
        (sample @dist))
      (bb-as-prior [mr]
        ;; The current belief is converted to a prior belief
        ;; by setting the sample count to 1.
        (if (<= cnt 1) mr
          (mean-reward-belief (/ sum cnt) (/ sum2 cnt) 1.))))))

(def initial-mean-reward-belief
  "uninformative mean reward belief"
  (mean-reward-belief 0. 0. 0.))

;;;; Bandit

(defrecord multiarmed-bandit [arms 
                              new-arm-belief 
                              new-arm-count 
                              new-arm-drawn])

(def fresh-bandit
  "bandit with no arm pulls"
  (map->multiarmed-bandit
   {:arms {}
    :new-arm-belief initial-mean-reward-belief
    :new-arm-count 0}))

;; An arm has two fields, :belief and :count. :count is the number
;; of times the arm was randomly selected from the prior as new.

;; Selects arms using open randomized probability matching.

(def ^:private +not-a-value+
  "value of new arm"
  ::not-a-value)

(defn not-a-value?
  "true when new value must be sampled"
  [value]
  (= value +not-a-value+))

(defn select-value
  [bandit log-p]
  ;; If the best arm happens to be a new arm,
  ;; return nil. checkpoint [::algorithm sample]
  ;; accounts for this and samples a new value
  ;; from the prior.
  (if (empty? (seq (:arms bandit))) +not-a-value+
    (loop [arms (:arms bandit)
           best-reward (/ -1. 0.)
           best-value +not-a-value+
           best-belief nil]
      (if-let [[[value {:keys [belief count]}] & arms] (seq arms)]
        (let [reward (+ (log-p value) 
                        (reduce max
                                (repeatedly
                                  count #(bb-sample belief))))]
          (if (>= reward best-reward)
            (recur arms reward value belief)
            (recur arms best-reward best-value best-belief)))
        ;; Select a new arm with the probability
        ;; that the arm has the highest mean reward.
        (loop [arms (:arms bandit)
               best-reward (+ (log-p best-value)
                              (bb-sample best-belief))
               best-value +not-a-value+]
          (if-let [[[value {:keys [belief count]}] & arms] (seq arms)]
            (let [reward (+ (log-p value)
                            (reduce max
                                    (repeatedly
                                      count #(bb-sample belief))))]
              (if (> reward best-reward)
                (recur arms reward value)
                (recur arms best-reward best-value)))
            best-value))))))

(defn update-bandit
  "updates bandit's belief"
  [bandit value reward]
  (let [bandit (if (:new-arm-drawn bandit)
                 ;; A new arm was drawn, which may or may not
                 ;; coincide with an existing arm.
                 (-> bandit 
                     (update-in [:new-arm-belief] bb-update reward)
                     (update-in [:new-arm-count] inc)
                     (update-in [:arms value :count]
                                (fnil inc 0)))
                 bandit)]
    ;; Update the belief about the mean reward of the sampled arm.
    (update-in bandit [:arms value :belief]
               (fnil bb-update
                     ;; If the arm is new, derive the belief from
                     ;; the belief about a randomly drawn arm.
                     (bb-as-prior (:new-arm-belief bandit)))
               reward)))

;;; Trace

;; The trace is a vector of tuples
;;   [bandit-id value past-reward]
;; where past reward is the reward accumulated before
;; reaching this random choice.

(defrecord entry [bandit-id value past-reward])

(defn bandit-id [smp state]
  "returns bandit id for the checkpoint and the updated-state"
  (checkpoint-id smp state ::bandit-counts ::bandit-last-id))

(defn record-choice
  "records random choice in the state"
  [state bandit-id value past-reward]
  (update-in state [::trace]
             conj (->entry bandit-id value past-reward)))

;;;; MAP inference

(defmethod checkpoint [::algorithm embang.trap.sample] [algorithm smp]
  (let [state (:state smp)
        [bandit-id state] (bandit-id smp state)
        bandit ((state ::bandits) bandit-id fresh-bandit)

        ;; Select a value as a bandit arm.
        value (select-value bandit #(observe (:dist smp) %))
        ;; Remember whether a new arm was drawn;
        ;; new arm belief is updated during back-propagation.
        bandit (assoc bandit :new-arm-drawn (not-a-value? value))
        ;; Sample a new value if a new arm was drawn.
        value (if (not-a-value? value)
                (sample (:dist smp))
                value)

        ;; Past reward is the reward collected upto
        ;; the current sampling point.
        past-reward (get-log-weight state)
        ;; Edge reward is the probability of the sample
        ;; conditioned on the prefix
        edge-reward (observe (:dist smp) value)
        ;; Update the state:
        state (-> state
                  ;; Increment the log weight by the probability
                  ;; of the sampled value.
                  (add-log-weight edge-reward)
                  ;; Re-insert the bandit, the bandit may be fresh,
                  ;; and the new-arm-drawn flag may have been updated.
                  (assoc-in [::bandits bandit-id] bandit)
                  ;; Insert an entry for the random choice into the trace.
                  (record-choice bandit-id value past-reward))]
    ;; Finally, continue the execution.
    #((:cont smp) value state)))

;; Backpropagating rewards

(defn backpropagate
  "back propagate reward to bandits"
  [state]
  (let [reward (get-log-weight state)]
    (if (< (/ -1. 0.) reward (/ 1. 0.))

      ;; Detach the trace and the bandits from the existing
      ;; states, update the bandits and reattach them to
      ;; the initial state.
      (loop [trace (state ::trace)
             bandits (state ::bandits)]
        (if (seq trace)
          (let [[{:keys [bandit-id value past-reward]} & trace] trace]
            (recur
              trace
              (update-in bandits [bandit-id]
                         ;; Bandit arms grow incrementally.
                         update-bandit value (- reward past-reward))))
          (assoc initial-state ::bandits bandits)))

      ;; If the reward is not meaningful, drop it and
      ;; carry over the bandits.
      (assoc initial-state
             ::bandits (state ::bandits)))))

;;; Reporting the mode

(defn add-trace-predict
  "adds trace as a predict"
  [state]
  (add-predict state '$trace
               (map :value (::trace state))))

;;; Reporting the internal statistics

(defn add-bandit-predict
  "add bandit arms and counts as a predict"
  [state]
  (add-predict state '$bandits
               (sort-by
                 first
                 (map (fn [[bandit-id {:keys [arms new-arm-count]}]]
                        ;; For each bandit, report the number of
                        ;; arts and the number of times a new arm
                        ;; was chosen.
                        [bandit-id {:arm-count (count arms)
                                    :new-arm-count new-arm-count}])
                      (state ::bandits)))))

;;; Inference method

(defmethod infer :bgrad [_ prog value
                         & {:keys [predict-trace
                                   predict-candidates
                                   predict-bandits
                                   number-of-samples]
                            :or {predict-trace false
                                 predict-candidates false
                                 predict-bandits false}}]
  ;; The MAP inference consists of two chained transformations,
  ;; `sample-seq', followed by `map-seq'.
  (letfn
    [(sample-seq [state]
       (lazy-seq
         (let [state (:state (exec ::algorithm prog value 
                                   (backpropagate state)))
               state (if predict-trace
                       (add-trace-predict state)
                       state)
               state (if predict-bandits
                       (add-bandit-predict state)
                       state)]
           (cons state
                 (sample-seq state)))))

     (map-seq [sample-seq max-log-weight]
       ;; Filters MAP estimates by increasing weight.
       (lazy-seq
         (when-let [[sample & sample-seq] (seq sample-seq)]
           (if (or predict-candidates
                   (> (get-log-weight sample) max-log-weight))
             (cons sample
                   (map-seq sample-seq (get-log-weight sample)))
             (map-seq sample-seq max-log-weight)))))]

    (let [sample-seq (sample-seq
                       (:state
                         (exec ::algorithm prog value initial-state)))
          sample-seq (if number-of-samples
                       (take number-of-samples sample-seq)
                       sample-seq)]
      (map-seq sample-seq (Math/log 0.)))))
