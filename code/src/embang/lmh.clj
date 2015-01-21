(ns embang.lmh
  (:use [embang.state :exclude [initial-state]]
        embang.inference
        [embang.runtime :only [observe sample]]))

;;;; Lightweight (single-site) Metropolis-Hastings

(derive ::algorithm :embang.inference/algorithm)

;;; Initial state

(def initial-state
  "initial state for LMH"
  (into embang.state/initial-state
        ;; The state is extended by the trace ---
        ;; the vector of current random choices,
        ;; and the random database --- random choices
        ;; from the previous particle.
        {::trace []       ; current random choices
         ::rdb {}         ; stored random choices
         ::counts {}      ; counts of occurences of each `sample'
         ::last-id nil})) ; last sample id

;;; Trace

;; ASMH need access to the trace, expose it via an accessor function.

(defn get-trace "returns trace" [state] (state ::trace))

;; The trace is a vector of entries
;;   {choice-id value log-p cont}
;; where
;;   - `choice-id' is the identifier of the random choice
;;   - `value' is the value of random choice in the current
;;     run,
;;   - `log-p' is the log probability (mass or density) of
;;     the value given the distribution,
;;   - `cont' is the continuation that starts at the checkpoint.

(defrecord entry [choice-id value log-p cont])

(defn record-random-choice
  "records random choice in the state"
  [state choice-id value log-p cont]
  (let [sample-id (first choice-id)]
    (-> state
        (update-in [::trace]
                   conj (->entry choice-id value log-p cont))
        (update-in [::counts sample-id]
                   ;; If the count is positive but the last sample-id
                   ;; is different, pad the count to decrease
                   ;; the probability of address derailing.
                   (fn [count]
                     (inc (cond
                            (nil? count) 0
                            (not= sample-id
                                  (state ::last-id)) (bit-or count 15)
                            :else count))))
        (assoc-in [::last-id] sample-id))))

;; choice-id is a tuple
;;  [sample-id number-of-previous-occurences]
;; so that different random choices get different ids.

(defn choice-id
  "returns choice id for the sample checkpoint"
  [smp state]
  [(:id smp) ((state ::counts) (:id smp) 0)])

;;; Random database (RDB)

;; RDB is a mapping from choice-ids to the chosen values.

(defn rdb
  "creates random database from trace"
  [trace]
  (into {} (map (comp vec (juxt :choice-id :value)) trace)))

;;; Inference

(defmethod checkpoint [::algorithm embang.trap.sample] [_ smp]
  (let [state (:state smp)
        choice-id (choice-id smp state)
        value (if (contains? (state ::rdb) choice-id)
                ((state ::rdb) choice-id)
                (sample (:dist smp)))
        log-p (try (observe (:dist smp) value)
                   ;; NaN is returned if value is not in support.
                   (catch Exception e (/ 0. 0.)))
        value (if (< (/ -1. 0.) log-p (/ 1. 0.)) value
                ;; The retained value is not in support,
                ;; resample the value from the prior.
                (sample (:dist smp)))
        cont (fn [_ update]
               ;; Continuation which starts from this checkpoint
               ;; --- called when the random choice is selected
               ;; for resampling.
               (update-in smp [:state]
                          ;; Update fields override state fields.
                          (fn [state]
                            (merge-with #(or %2 %1) state update))))
        state (record-random-choice state
                                    choice-id value log-p cont)]
    #((:cont smp) value state)))

;;; State transition

;; Optional `update' argument is used by ASMH to override
;; additional fields. The state is extensible, so are
;; state transformation methods.

(defn next-state
  "produces next state given current state
  and the trace entry to resample"
  ([state entry] (next-state state entry {}))
  ([state entry update]
   (:state (exec ::algorithm (:cont entry) nil 
                 ;; Remove the selected entry from RDB.
                 (into update
                       {::rdb (dissoc (rdb (state ::trace))
                                      (:choice-id entry))})))))

(defn prev-state
  "produces previous state given the current and
  the next state and the resampled entry
  by re-attaching new rdb to the original state"
  ([state next-state entry] (prev-state state next-state entry {}))
  ([state next-state entry update]
   (merge-with #(or %2 %1) state
               (into update
                     {::rdb (dissoc (rdb (next-state ::trace))
                                    (:choice-id entry))}))))

;; Transition probability

(defn get-log-retained
  "computes log probability of retained random choices"
  [state]
  (reduce + (keep
              (fn [{:keys [choice-id value log-p]}]
                (when (and (contains? (state ::rdb) choice-id)
                           (= value ((state ::rdb) choice-id)))
                  log-p))
              (state ::trace))))

(defn utility
  "computes state utility, used to determine
  the acceptance log-probability as (next-utility - prev-utility)"
  [state]
  (+ (get-log-weight state)
     (get-log-retained state)
     (- (Math/log (count (state ::trace))))))

(defmethod infer :lmh [_ prog & {}]
  (letfn
    [(sample-seq [state]
       (lazy-seq
         (let [;; Choose uniformly a random choice to resample.
               entry (rand-nth (state ::trace))
               ;; Compute next state from the resampled choice.
               next-state (next-state state entry)
               ;; Reconstruct the current state through transition back
               ;; from the next state; the rdb will be different.
               prev-state (prev-state state next-state entry)
               ;; Apply Metropolis-Hastings acceptance rule to select
               ;; either the new or the current state.
               state (if (> (- (utility next-state) (utility prev-state))
                            (Math/log (rand)))
                       next-state
                       state)]
           ;; Include the selected state into the sequence of samples,
           ;; setting the weight to the unit weight.
           (cons (set-log-weight state 0.) (sample-seq state)))))]
    (sample-seq (:state (exec ::algorithm prog nil initial-state)))))
