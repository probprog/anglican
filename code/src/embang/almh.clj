(ns embang.almh
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:use clojure.pprint)
  (:use [embang.state :exclude [initial-state]]
        embang.inference
        [embang.lmh :exclude [initial-state
                              next-state prev-state
                              utility]]))

;;;; Adaptive scheduling single-site Metropolis-Hastings
;
;; A separate set of pending entries is maintained
;; for each predict, and the reward is distributed
;; between the entries when the predict is updated.

;; The code here deliberately inherits from LMH. I wanted
;; to find a compromise between reproducing much of the
;; LMH code and forcing the reader to jump back and forth
;; between two source files.

(derive ::algorithm :embang.lmh/algorithm)

;;; Initial state

(def initial-state
  "initial state for ALMH"
  (into embang.lmh/initial-state
        {::choice-rewards {}
         ::last-predicts {}
         ::choice-counts {}}))

;;; Algorithm parameters

(def ^:private +exploration-factor+
  "UCB exploration factor"
  0.1)

;;; Choice reward 

;; Choice reward is a tuple [sum count] of normalized
;; total reward and total weight.

(def ^:private  +prior-choice-reward+
  "reward of an unseen arm"
  [1. 1.])

(defn update-reward
  "updates choice reward with new evidence"
  [[sum cnt] reward discnt]
  [(+ sum (* discnt reward)) (+ cnt discnt)])

(defn update-rewards 
  "updates rewards in pending choices;
  returns updated choice-rewards"
  [choice-rewards pending-choices reward discnt]
  (reduce (fn [choice-rewards choice-id]
            (update-in choice-rewards [choice-id]
                       (fnil update-reward
                             +prior-choice-reward+)
                       reward discnt))
          choice-rewards pending-choices))

;;; Stored predict for reward distribution.

;; Predicts are stored in a map indexed by predict label.
;; Each predict record contains the last predict value
;; and a list of pending choice ids --- those which were
;; selected after the last change of predict's value.

(defrecord predict [value     ; predicted value
                    choices]) ; pending choices

(def ^:private +not-a-predict+ 
  "a value different from any possible predict,
  for non-global predicts missing in the state"
  ::not-a-predict)

;;; State transition

(defn combined-predicts
  "combines current predicts with last predicts
  for which no current predict is available, assigning
  the latters +not-a-predict+ as the value;
  returns combined predicts"
  [state]
  (loop [predicts (get-predicts state)
         last-predicts (state ::last-predicts)]
    (if-let [[[label _] & predicts] (seq predicts)]
      (recur predicts
             (dissoc last-predicts label))
      (concat (get-predicts state)
              (map (fn [[label _]]
                     [label +not-a-predict+])
                   last-predicts)))))

(defn award
  "distributes rewards between random choices;
  returns updated state"
  [state entry]
  (let [choice-id (:choice-id entry)
        predicts (combined-predicts state)
        discnt (/ 1. (double (count predicts)))]

    (loop [choice-rewards (state ::choice-rewards)
           last-predicts (state ::last-predicts)
           predicts predicts]
      (if-let [[[label value] & predicts] (seq predicts)]

        ;; Append the new choice to the list of pending choices.
        (let [pending-choices (conj (:choices (last-predicts label))
                                    choice-id)
              discnt (/ discnt (double (count pending-choices)))]
          (if (= value (:value (last-predicts label)))
            ;; Same predict, append the choice to the collection.
            (recur
              (update-rewards choice-rewards pending-choices 0. discnt)
              (assoc-in last-predicts [label :choices] pending-choices)
              predicts)

            ;; Different predict, update rewards for pending choices.
            (recur
              (update-rewards choice-rewards pending-choices 1. discnt)
              (-> last-predicts
                  (update-in [label :choices] empty)
                  (assoc-in [label :value] value))
              predicts)))

        ;; Finally, record new rewards and predicts in the state.
        (assoc state
               ::choice-rewards choice-rewards
               ::last-predicts last-predicts)))))

(defn update-choice-count
  "updates total count for the chosen entry"
  [state entry]
  (update-in state [::choice-counts
                    (:choice-id entry)]
             (fnil inc 0)))

(defn state-update
  "computes state update --- fields that
  must be transferred to the state before
  running random choice continuation"
  [state]
  (into {} (map (fn [k] [k (state k)])
                [::choice-rewards
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
                                  [choice-id (/ sum cnt) cnt])
                                (state ::choice-rewards))))
      ;; Actual choice counts, these are different
      ;; from normalized discounted counts in rewards.
      (add-predict '$choice-counts
                   (sort-by first (state ::choice-counts)))
      (add-predict '$total-count
                   (reduce + (map second
                                  (state ::choice-counts))))))

(defmethod infer :almh [_ prog & {:keys [predict-choices]
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
                         ;; according to changes in predicts to favor
                         ;; choices which affect more predicts.
                         (award next-state entry)
                         ;; The old state is held. Award 0 reward to
                         ;; the choice that caused reject to increase
                         ;; acceptance rate.
                         (update-in state
                                    [::choice-rewards (:choice-id entry)]
                                    (fnil update-reward
                                          +prior-choice-reward+)
                                    0. 1.))
                 ;; In any case, update the entry count.
                 state (update-choice-count state entry)

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

    (let [;; Run the first particle.
          state (:state (exec ::algorithm prog nil initial-state))
          ;; Initialize the predict table.
          state (assoc state ::last-predicts
                       (into {} (map (fn [[label value]]
                                       [label (->predict value nil)])
                                     (get-predicts state))))]
      (sample-seq state))))
