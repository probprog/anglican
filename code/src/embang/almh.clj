(ns embang.almh
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:use [embang.state :exclude [initial-state]]
        embang.inference
        [embang.lmh :exclude [initial-state
                              next-state prev-state
                              utility]]))

;;;; Adaptive scheduling single-site Metropolis-Hastings
;;
;; A separate set of pending entries is maintained
;; for each predict, and the reward is distributed
;; between the entries when the predict is updated.

;; The code here deliberately inherits from LMH. I wanted
;; to find a compromise between reproducing much of the
;; LMH code and forcing the reader to jump back and forth
;; between two source files.

(derive ::algorithm :embang.lmh/algorithm)

;;; Algorithm parameters

(def ^:private +exploration-factor+
  "UCB exploration factor"
  ;; 0.5 is going to work well in most cases.
  ;; A lower value would favour exploitation.
  0.5)

;;; Initial state

(def initial-state
  "initial state for ALMH"
  (into embang.lmh/initial-state
        {::choice-rewards {}
         ::last-predicts {}
         ::choice-counts {}}))

;;; Stored predicts for reward distribution.

;; Predicts are stored in a map indexed by predict label.
;; Each predict record contains the last predict value
;; and all pending choice ids --- those which were
;; selected after the last change of predict's value.

(defrecord predict [value     ; predicted value
                    choices]) ; pending choices

(def ^:private +not-a-predict+ 
  "a value different from any possible predict,
  for non-global predicts missing in the state"
  ::not-a-predict)

(defn initialize-last-predicts
  "initializes last predicts from current predicts,
  called on the first end state;
  returns updated state"
  [state]
  (assoc state ::last-predicts
         (into {} (map (fn [[label value]]
                         [label (->predict value {})])
                       (get-predicts state)))))

;; Pending choices is a map choice-id -> choice count.

(defn add-pending-choice
  "adds a pending choice to the predict"
  [predict choice-id]
  (update-in predict [:choices choice-id] (fnil inc 0)))

;;; Choice reward 

;; Choice reward is a tuple [sum count] of normalized
;; total reward and total weight.

(def ^:private +prior-choice-reward+
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
  ;; Distribute the reward equally between pending choices.
  (let [discnt (/ discnt (reduce + (vals pending-choices)))]
    (reduce (fn [choice-rewards [choice-id choice-cnt]]
              (update-in choice-rewards [choice-id]
                         (fnil update-reward
                               +prior-choice-reward+)
                         reward (* choice-cnt discnt)))
            choice-rewards pending-choices)))

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

    (loop [predicts predicts
           choice-rewards (state ::choice-rewards)
           last-predicts (state ::last-predicts)]
      (if-let [[[label value] & predicts] (seq predicts)]

        ;; Append the new choice to the list of pending choices.
        (let [last-predicts (update-in last-predicts [label]
                                       add-pending-choice choice-id)]
          (if (= value (:value (last-predicts label)))
            ;; Same predict, penalize pendings choices.
            (recur
              predicts
              (update-rewards
                ;; On average, assigning 0/1 to the just arrived
                ;; element is tantamount to distributing
                ;; 0/|history|^-1 to all elements in the queue,
                ;; but faster.
                choice-rewards {choice-id 1}
                0. discnt)
              last-predicts)

            ;; Different predict, reward pending choices.
            (recur
              predicts
              (update-rewards
                choice-rewards (:choices (last-predicts label))
                1. discnt)
              ;; Flush pending choices and update the value.
              (-> last-predicts
                  (update-in [label :choices] empty)
                  (assoc-in [label :value] value)))))

        ;; Finally, record new rewards and predicts in the state.
        (assoc state
               ::choice-rewards choice-rewards
               ::last-predicts last-predicts)))))

(defn update-choice-count
  "updates total count for the chosen entry"
  [state entry]
  (update-in state [::choice-counts (:choice-id entry)]
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

;;; Adaptive scheduling

(defn ucb
  "returns function computing UCB of average reward"
  [total-count]
  (fn [[sum cnt]]
    ;; b_i = \overline r_i + C \sqrt {\frac {log N} {N_i}}
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
                       ;; Find the entry's weight.
                       (fn [[choice-id weight]]
                         (when (= choice-id (:choice-id entry))
                           weight))
                       ;; Zip entry ids and weights.
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

;;; Reporting statistics

;; Sample identifiers can be computed at runtime
;; and may be of any comparable type. Sort them
;; as strings.

(let [sort-choices (partial sort-by (comp str first))]

  (defn add-choice-predicts
    "adds predicts for choice statistics
    to the state"
    [state]
    (-> state
        ;; Average choice rewards
        (add-predict '$choice-rewards
                     (sort-choices
                       (map (fn [[choice-id [sum cnt]]]
                              [choice-id (/ sum cnt) cnt])
                            (state ::choice-rewards))))
        ;; Actual choice counts, these are different
        ;; from normalized discounted counts in rewards.
        (add-predict '$choice-counts
                     (sort-choices (state ::choice-counts)))
        (add-predict '$total-count
                     (reduce + (map second
                                    (state ::choice-counts)))))))

(defmethod infer :almh [_ prog value
                        & {:keys [predict-choices] ; report statistics
                           :or {predict-choices false}}]
  (letfn
    [(sample-seq [state]
       (lazy-seq
         (let [;; Choose uniformly a random choice to resample.
               entry (select-entry state)

               ;; Compute next state from the resampled choice.
               next-state (next-state state entry)
               ;; Reconstruct the current state through transition
               ;; back from the next state, with a different rdb.
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

                       ;; The old state is held.
                       state)

               ;; In any case, update the entry count.
               state (update-choice-count state entry)

               ;; Include the selected state into the sequence of
               ;; samples, setting the weight to the unit weight.
               sample (set-log-weight state 0.)
               ;; Optionally, add rewards and counts to predicts.
               sample (if predict-choices
                        (add-choice-predicts sample)
                        sample)]
           (cons sample (sample-seq state)))))]

    (let [;; Run the first particle.
          state (:state (exec ::algorithm prog value initial-state))
          ;; Initialize the predict table.
          state (initialize-last-predicts state)]
      (if (seq (get-trace state))
        (sample-seq state)
        ;; No randomness in the program
        (repeat (set-log-weight state 0.))))))
