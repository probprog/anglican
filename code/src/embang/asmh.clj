(ns embang.asmh
  (:use clojure.pprint)
  (:use [embang.state :exclude [initial-state]]
        embang.inference
        [embang.lmh :exclude [initial-state
                              next-state prev-state
                              utility]]))

;;;; Adaptive scheduling single-site Metropolis-Hastings

;; The code here deliberately inherits from LMH. I wanted
;; to find a compromise between reproducing much of the code
;; of LMH and saving the reader from jumping back and forth
;; between two source files.

(derive ::algorithm :embang.lmh/algorithm)

;;; Initial state

(def initial-state
  "initial state for ASMH"
  (into embang.lmh/initial-state
        {::choice-rewards {}
         ::choice-history ()
         ::last-predicts {}}))

(def ^:private +history-size+ 
  "number of past choices to keep in the history"
  16)

(def ^:private +reward-discount+
  "applied to rewards to backpropagate the evidence"
  0.5)

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
  (let [history (take +history-size+ 
                      (cons (:choice-id entry)
                            (::choice-history state)))

        rewards (loop [rewards (state ::choice-rewards)
                       discount +reward-discount+
                       history history]
                  (if-let [[choice-id & history] (seq history)]
                    (recur
                      (update-in
                        rewards [choice-id]
                        (fnil (fn [[sum cnt]]
                                [(+ sum (* discount reward))
                                 (+ cnt discount)])
                              [0. 0.]))
                      (* discount +reward-discount+)
                      history)
                    rewards))]
    (assoc state
           ::choice-rewards rewards
           ::choice-history history
           ::last-predicts (into {} (get-predicts state)))))

(defn state-update
  "computes state update --- fields that
  must be transferred to the state before
  running random choice continuation"
  [state]
  (into {} (map (fn [k] [k (state k)])
                [::choice-rewards
                 ::choice-history
                 ::last-predicts])))

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

(defmethod infer :asmh [_ prog & {:keys [predict-rewards]
                                  :or {predict-rewards false}}]
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
                       ;; The old state is held --- award choices
                       ;; in the history zero reward.
                       (award state entry 0.))]
           ;; Include the selected state into the sequence of samples,
           ;; setting the weight to the unit weight.
           (cons (set-log-weight state 0.) (sample-seq state)))))]
    (sample-seq (:state (exec ::algorithm prog nil initial-state)))))
