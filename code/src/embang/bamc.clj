(ns embang.bamc
  "Bayesian ascent Monte Carlo
   Options:
     :predict-candidates (false by default)
       - output all samples rather than just those
         with increasing log-weight"
  (:refer-clojure :exclude [rand rand-int rand-nth])
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
  (bb-sample-mean [belief]
    "returns a random sample from the mean belief distribution")
  (bb-as-prior [belief]
    "returns a belief for use as a prior belief"))

;;;; Mean reward belief

(defn reward-belief
  "returns reification of bayesian belief
  about the mean reward of an arm"
  [sum sum2 ^double cnt]
  ;; Bayesian belief about the reward (log-weight).
  (let [mean (/ sum cnt)
        sd (Math/sqrt (- (/ sum2 cnt) (* mean mean)))
        mean-sd (/ sd (Math/sqrt cnt))
        dist (normal mean sd)
        mean-dist (normal mean mean-sd)]
  (reify bayesian-belief
      (bb-update [rb reward]
        (reward-belief
          (+ sum reward) (+ sum2 (* reward reward)) (+ cnt 1.)))
      (bb-sample [rb] (sample dist))
      (bb-sample-mean [rb] (sample mean-dist))
      (bb-as-prior [rb]
        ;; The current belief is converted to a prior belief
        ;; by setting the sample count to 1 and preserving
        ;; the mean and variance.
        (if (<= cnt 1.) rb
          (reward-belief (/ sum cnt) (/ sum2 cnt) 1.))))))

(def initial-reward-belief
  "uninformative mean reward belief"
  (reward-belief 0. 0. 0.))

;;;; Bandit

(defrecord multiarmed-bandit [arms
                              new-arm-belief
                              new-arm-count
                              new-arm-drawn])
(def fresh-bandit
  "bandit with no arm pulls"
  (map->multiarmed-bandit
   {:arms {}
    :new-arm-belief initial-reward-belief
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
  "selects value corresponding to the best arm"
  [bandit log-p]
  ;; If the best arm happens to be a new arm,
  ;; return +not-a-value+. Checkpoint [::algorithm sample]
  ;; accounts for this and samples a new value
  ;; from the prior.
  (if (empty? (seq (:arms bandit))) +not-a-value+
    (loop [arms (:arms bandit)
           best-reward (/ -1. 0.)
           best-value +not-a-value+
           best-belief nil]
      ;; First, we choose the new arm candidate proportional
      ;; to the probability that the reward drawn from the
      ;; arm is the maximum one.
      (if-let [[[value {:keys [belief count]}] & arms] (seq arms)]
        (let [reward (+ (log-p value)
                        (reduce max
                                (repeatedly
                                  count #(bb-sample belief))))]
          (if (>= reward best-reward)
            (recur arms reward value belief)
            (recur arms best-reward best-value best-belief)))
        ;; Then, we select an arm with the probability
        ;; that the arm has the highest *average* reward,
        ;; including the new arm candidate.
        (loop [arms (:arms bandit)
               best-reward (+ (log-p best-value)
                              (bb-sample-mean best-belief))
               best-value +not-a-value+
               parity 0.] ; number of 
          (if-let [[[value {:keys [belief count]}] & arms] (seq arms)]
            (let [reward (+ (log-p value)
                            (reduce max
                                    (repeatedly
                                      count #(bb-sample-mean belief))))]
              (cond
                (> reward best-reward)
                (recur arms reward value parity)

                (= reward best-reward)
                (let [parity (+ parity (double count))]
                  (if (> (/ parity) (rand))
                    (recur arms reward value parity)
                    (recur arms best-reward best-value parity)))

                :else
                (recur arms best-reward best-value parity)))
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

        ;; Select value:

        ;; Select a value as a bandit arm.
        value (select-value bandit #(observe (:dist smp) %))
        ;; Remember whether a new arm was drawn;
        ;; new arm belief is updated during back-propagation.
        bandit (assoc bandit :new-arm-drawn (not-a-value? value))
        ;; Sample a new value if a new arm was drawn.
        value (if (not-a-value? value)
                (sample (:dist smp))
                value)

        ;; Update state:

        ;; Increment the log weight by the probability
        ;; of the sampled value.
        state (add-log-weight state (observe (:dist smp) value))
        ;; Re-insert the bandit; the bandit may be fresh,
        ;; and the new-arm-drawn flag may have been updated.
        state (assoc-in state [::bandits bandit-id] bandit)
        ;; Insert an entry for the random choice into the trace.
        state (record-choice
               state bandit-id value (get-log-weight state))]

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
                        ;; arms and the number of times a new arm
                        ;; was chosen.
                        [bandit-id {:arm-count (count arms)
                                    :new-arm-count new-arm-count}])
                      (state ::bandits)))))

;;; Inference method

(defmethod infer :bamc
  [_ prog value
   & {:keys [;; Add the trace as a predict.
             predict-trace
             ;; Output all states rather than just states
             ;; with increasing log-weight.
             predict-candidates
             ;; Report internal statistics.
             predict-bandits
             ;; Total number of samples to produce.
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
