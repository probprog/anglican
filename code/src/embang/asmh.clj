(ns embang.asmh
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

;;; Choice reward 

;; Choice reward is a tuple [sum count] of normalized
;; total reward and total weight.

(def ^:private  +prior-choice-reward+
  "reward of an unseen arm"
  [+reward-discount+ +reward-discount+])

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
  ([state entry reward] (award state entry reward 1.))
  ([state entry reward weight]
   (let [ ;; Push new choice into the history.
         history (take +history-size+
                       (cons (:choice-id entry)
                             (::choice-history state)))

         ;; Distribute discount reward among choices 
         ;; in the history.
         rewards (loop [rewards (state ::choice-rewards)
                        discount (* +reward-discount+ weight)
                        history history]
                   (if-let [[choice-id & history] (seq history)]
                     (recur (update-in rewards [choice-id]
                                       (fnil update-choice-reward
                                             +prior-choice-reward+)
                                       reward discount)
                            (* discount +reward-discount+)
                            history)
                     rewards))]
     ;; Finally, record new rewards, history, predicts, and
     ;; counts in the state.
     (-> state
         (assoc ::choice-rewards rewards
                ::choice-history history
                ::last-predicts (into {} (get-predicts state)))
         (update-in [::choice-counts (:choice-id entry)]
                    (fnil inc 0))))))

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

;; Transition probability

(defn utility
  "computes state utility, used to determine
  the acceptance log-probability as (next-utility - prev-utility)"
  [state entry]
  (+ (get-log-weight state)
     (get-log-retained state)
     (- (Math/log (count (get-trace state))))))

;; Reporting statistics

(defn add-choice-predicts
  "adds predicts for choice statistics
  to the state"
  [state]
  (-> state
      (add-predict '$choice-rewards
                   (sort-by first
                           (map (fn [[choice-id [rwd cnt]]]
                                  [choice-id (/ rwd cnt)])
                                (state ::choice-rewards))))
      (add-predict '$choice-counts
                   (sort-by first (state ::choice-counts)))
      (add-predict '$total-count
                   (reduce + (map second
                                  (state ::choice-counts))))))

(defmethod infer :asmh [_ prog & {:keys [predict-choices
                                         punish-rejects]
                                  :or {predict-choices false
                                       punish-rejects false}}]
  (letfn
    [(sample-seq [state]
       (lazy-seq
         (let [;; Choose uniformly a random choice to resample.
               entry (rand-nth (get-trace state))

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

                       ;; The old state is held:
                       ;;   - either award choices zero reward,
                       ;;   - or just ignore the rejected update.
                       (award state entry 0. (if punish-rejects 1. 0.)))

               ;; Include the selected state into the sequence of samples,
               ;; setting the weight to the unit weight.
               sample (set-log-weight state 0.)
               ;; Optionally, add rewards and counts to predicts.
               sample (if predict-choices
                        (add-choice-predicts sample)
                        sample)]
           (cons sample (sample-seq state)))))]
    (sample-seq (:state (exec ::algorithm prog nil initial-state)))))
