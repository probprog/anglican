(ns anglican.lmh
  "Lightweight Metropolis-Hastings"
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:use [anglican.state :exclude [initial-state]]
        anglican.inference
        [anglican.runtime :only [observe* sample*]]))

;;;; Lightweight (single-site) Metropolis-Hastings

(derive ::algorithm :anglican.inference/algorithm)

;;; Initial state

(def initial-state
  "initial state for LMH"
  (into anglican.state/initial-state
        ;; The state is extended by the trace ---
        ;; the vector of current random choices,
        ;; and the random database --- random choices
        ;; from the previous particle.
        {::trace []               ; current random choices
         ::rdb {}                 ; stored random choices
         ::choice-counts {}       ; counts of occurences of each `sample'
         ::choice-last-id nil}))  ; last sample id

;;; Trace

;; ALMH need access to the trace, expose it via an accessor function.
(defn get-trace "returns trace" [state] (state ::trace))

;; The trace is a vector of entries
;;   {choice-id value log-p cont}
;; where
;;   - `choice-id' is the identifier of the random choice,
;;   - `value' is the value of random choice in the current run,
;;   - `log-p' is the log probability (mass or density) of
;;     the value given the distribution,
;;   - `cont' is the continuation that starts at the checkpoint.

(defrecord entry [choice-id value log-p cont])

(defn choice-id
  "returns a unique idenditifer for sample checkpoint
  and the updated state"
  [smp state]
  (checkpoint-id smp state ::choice-counts ::choice-last-id))

(defn record-choice
  "records random choice in the state"
  [state choice-id value log-p cont]
  (update-in state [::trace]
             conj (->entry choice-id value log-p cont)))

;;; Random database (RDB)

;; RDB is a mapping from choice-ids to the chosen values.

(defn rdb
  "creates random database from trace"
  [trace]
  (into {} (map (fn [entry]
                  [(:choice-id entry) (:value entry)])
                trace)))

;;; Inference

(defmethod checkpoint [::algorithm anglican.trap.sample] [_ smp]
  (let [[choice-id state] (choice-id smp (:state smp))
        value (if (contains? (state ::rdb) choice-id)
                ((state ::rdb) choice-id)
                (sample* (:dist smp)))
        log-p (try (observe* (:dist smp) value)
                   ;; NaN is returned if value is not in support.
                   (catch Exception e (/ 0. 0.)))
        value (if (< (/ -1. 0.) log-p (/ 1. 0.)) value
                ;; The retained value is not in support, resample
                ;; the value from the prior.  When the value is
                ;; resampled, log-p is no longer valid, but log-p
                ;; of a resampled value is ignored anyway (see
                ;; `utility' below).
                (sample* (:dist smp)))
        cont (fn [_ update]
               ;; Continuation which starts from this checkpoint
               ;; --- called when the random choice is selected
               ;; for resampling.
               (update-in smp [:state]
                          ;; Update fields override state fields.
                          (fn [state]
                            (merge-with #(or %2 %1) state update))))
        state (record-choice state choice-id value log-p cont)]
    #((:cont smp) value state)))

;;; State transition

;; Optional `update' argument is used by Adaptive LMH to supply
;; additional fields. The state is extensible, so are state
;; transformation methods.

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

(defn get-log-retained-probability
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
     (get-log-retained-probability state)
     (- (Math/log (count (state ::trace))))))

(defn accept?
  "compute acceptance ratio for a sample, given the new state utility
  and the old state utility"
  [u-next u-prev]
  (or (= u-prev (/ -1. 0.))
      (> (- u-next u-prev) (Math/log (rand)))))

(defn correct-log-weight
  "corrects log weight of a sample, setting it to 0
  if the sample is in the support, -Infinity otherwise"
  [state]
  (let [log-weight (get-log-weight state)
        corrected-log-weight (if (> log-weight (/ -1. 0.))
                               0. (/ -1. 0.))]
    (set-log-weight state corrected-log-weight)))

(defmethod infer :lmh [_ prog value & {}]
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
               state (if (accept? (utility next-state) (utility prev-state))
                       next-state
                       state)]
           ;; Include the selected state into the sequence of
           ;; samples, setting the weight to the unit weight if the
           ;; sample is in the support of the target distribution,
           ;; -Infinity otherwise.
           (cons (correct-log-weight state) (sample-seq state)))))]

    (let [state (:state (exec ::algorithm prog value initial-state))]
      (if (seq (state ::trace))
        (sample-seq state)
        ;; No randomness in the program.
        (repeat (correct-log-weight state))))))
