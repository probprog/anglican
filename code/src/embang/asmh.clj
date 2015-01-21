(ns embang.asmh
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:use clojure.pprint)
  (:use [embang.state :exclude [initial-state]]
        embang.inference
        [embang.lmh :exclude [initial-state
                              next-state prev-state
                              utility]]))

;;;; Adaptive scheduling single-site Metropolis-Hastings

;; The code here deliberately inherits from LMH. I wanted
;; to find a compromise between reproducing much of the
;; LMH code and forcing the reader to jump back and forth
;; between two source files.

(derive ::algorithm :embang.lmh/algorithm)

;;; Initial state

(def initial-state
  "initial state for ASMH"
  (into embang.lmh/initial-state
        {::choice-rewards {}
         ::choice-history ()
         ::last-predicts {}
         ::choice-counts {}}))

;;; Algorithm parameters

(def ^:private +history-size+
  "number of past choices to keep in the history"
  16)

(def ^:private +reward-discount+
  "applied to rewards to backpropagate the evidence"
  0.5)

(def ^:private +exploration-factor+
  "UCB exploration factor"
  0.1)

;;; Choice reward 

;; Choice reward is a tuple [sum count] of normalized
;; total reward and total weight.

(def ^:private  +prior-choice-reward+
  "reward of an unseen arm"
  [1. 1.])

(defn update-choice-reward
  "updates choice reward with new evidence"
  [[sum cnt] reward discount]
  [(+ sum (* discount reward)) (+ cnt discount)])

;;; State transition

(defn reward
  "computes reward for predicts in the state"
  [state]
  ;; The reward is `d' for each predict with a different
  ;; value, 0 otherwise.  The total reward is bounded
  ;; between 0 (all predicts are unchanged) and 1
  ;; (all predicts are different).
  (let [d (/ 1. (double (count (get-predicts state))))]
    (reduce
      (fn [sum [label value]]
        (if (= value
               ((state ::last-predicts) label))
          sum (+ sum d)))
      0. (get-predicts state))))

(defn award
  "distributes the reward between random choices;
  returns updated state"
  [state entry reward]
  (let [ ;; Push new choice into the history.
        history (take +history-size+
                      (cons (:choice-id entry)
                            (::choice-history state)))

        ;; Distribute discount reward among choices 
        ;; in the history.
        rewards (loop [rewards (state ::choice-rewards)
                       discount +reward-discount+
                       history history]
                  (if-let [[choice-id & history] (seq history)]
                    (recur (update-in rewards [choice-id]
                                      (fnil update-choice-reward
                                            +prior-choice-reward+)
                                      reward discount)
                           (* discount (- 1. +reward-discount+))
                           history)
                    rewards))]

    ;; Finally, record new rewards, history, predicts, and
    ;; counts in the state.
    (-> state
        (assoc ::choice-rewards rewards
               ::choice-history history
               ::last-predicts (into {} (get-predicts state)))
        (update-in [::choice-counts (:choice-id entry)]
                   (fnil inc 0)))))

(defn state-update
  "computes state update --- fields that
  must be transferred to the state before
  running random choice continuation"
  [state]
  (into {} (map (fn [k] [k (state k)])
                [::choice-rewards
                 ::choice-history
                 ::last-predicts
                 ::choice-counts])))

(defn next-state
  "produces next state given current state
  and the trace entry to resample"
  [state entry]
  (embang.lmh/next-state state entry
                         (state-update state)))

(defn prev-state
  "produces previous state given the current and
  the next state and the resampled entry
  by re-attaching new rdb to the original state"
  [state next-state entry]
  (embang.lmh/prev-state state next-state entry
                         (state-update next-state)))

;; Adaptive scheduling

(defn ucb
  "returns function computing UCB of average reward"
  [total-count]
  (fn [[sum cnt]]
    (+ (/ sum cnt) (* +exploration-factor+
                      (Math/sqrt (/ (Math/log total-count)
                                    cnt))))))

(defn trace-weights
  "constructs a vector of trace weights"
  [state]
  (let [trace-rewards (map (fn [{:keys [choice-id]}]
                             ((state ::choice-rewards) choice-id
                              +prior-choice-reward+))
                           (get-trace state))
        total-count (reduce + (map (fn [[_ cnt]] cnt) trace-rewards))]

    (map (ucb total-count) trace-rewards)))

(defn select-entry 
  "selects trace entry based on reward beliefs"
  [state]
  ((get-trace state) (rand-roulette (trace-weights state))))

(defn log-entry-probability
  "computes log probability of the entry given state"
  [state entry]
  (let [trace-weights (trace-weights state)
        entry-weight (some
                       ;; find the entry's weight
                       (fn [[choice-id weight]]
                         (when (= choice-id (:choice-id entry))
                           weight))
                       ;; zip entry ids and weights
                       (map (fn [entry weight]
                              [(:choice-id entry) weight])
                            (get-trace state) trace-weights))
        total-weight (reduce + trace-weights)] 
    (Math/log (/ entry-weight total-weight))))

;; Transition probability

(defn utility
  "computes state utility, used to determine
  the acceptance log-probability as (next-utility - prev-utility)"
  [state entry]
  (+ (get-log-weight state)
     (get-log-retained-probability state)
     (log-entry-probability state entry)))

;; Reporting statistics

(defn add-choice-predicts
  "adds predicts for choice statistics
  to the state"
  [state]
  (-> state
      ;; Average choice rewards
      (add-predict '$choice-rewards
                   (sort-by first
                           (map (fn [[choice-id [sum cnt]]]
                                  [choice-id (/ sum cnt)])
                                (state ::choice-rewards))))
      ;; Actual choice counts, these are different
      ;; from normalized discounted counts in rewards.
      (add-predict '$choice-counts
                   (sort-by first (state ::choice-counts)))
      (add-predict '$total-count
                   (reduce + (map second
                                  (state ::choice-counts))))))

(defmethod infer :asmh [_ prog & {:keys [predict-choices]
                                  :or {predict-choices false}}]
  (letfn
    [(sample-seq [state]
       (lazy-seq
         (if (seq (get-trace state))
           (let [;; Choose uniformly a random choice to resample.
                 entry (select-entry state)

                 ;; Compute next state from the resampled choice.
                 next-state (next-state state entry)
                 ;; Reconstruct the current state through transition back
                 ;; from the next state; the rdb will be different.
                 prev-state (prev-state state next-state entry)

                 ;; Apply Metropolis-Hastings acceptance rule to select
                 ;; either the new or the current state.
                 state (if (> (- (utility next-state entry)
                                 (utility prev-state entry))
                              (Math/log (rand)))

                         ;; The new state is accepted --- award choices
                         ;; in the history according to changes in
                         ;; predicts.
                         (award next-state entry (reward next-state))

                         ;; The old state is held --- award 1
                         ;; to increase sampling rate.
                         (award state entry 1.))

                 ;; Include the selected state into the sequence of samples,
                 ;; setting the weight to the unit weight.
                 sample (set-log-weight state 0.)
                 ;; Optionally, add rewards and counts to predicts.
                 sample (if predict-choices
                          (add-choice-predicts sample)
                          sample)]
             (cons sample (sample-seq state)))
           
           ;; No randomness in the program.
           (cons (set-log-weight state 0.) (sample-seq state)))))]

    (sample-seq (:state (exec ::algorithm prog nil initial-state)))))
