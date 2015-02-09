(ns embang.siman
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:use [embang.state :exclude [initial-state]]
        embang.inference
        [embang.runtime :only [observe sample]]))

;;;; Finding MAP of the trace using simulated annealing

(derive ::algorithm :embang.inference/algorithm)

;;; Initial state

(def initial-state
  "initial state for PGibbs protocol"
  (into embang.state/initial-state
        ;; the state is extended by two sequences,
        ;;   ::future-samples used by retained particle
        ;;   ::past-samples updated by allparticles
        {::trace []              ; current random choices
         ::rdb {}                ; stored random choices
         ::choice-counts {}      ; counts of occurences of each `sample'
         ::choice-last-id nil})) ; last sample id

;;; Trace

;; The trace is a vector of entries
;;   {choice-id value cont}
;; where
;;   - `choice-id' is the identifier of the random choice,
;;   - `value' is the value of random choice in the current run,
;;   - `cont' is the continuation that starts at the checkpoint.

(defrecord entry [choice-id value cont])

(defn choice-id
  "returns an unique idenditifer for sample checkpoint
  and the updated state"
  [obs state]
  (checkpoint-id obs state ::choice-counts ::choice-last-id))

(defn record-choice
  "records random choice in the state"
  [state choice-id value cont]
  (update-in state [::trace]
             conj (->entry choice-id value cont)))

;;; Random database (RDB)

;; RDB is a mapping from choice-ids to the chosen values.

(defn rdb
  "creates random database from trace"
  [trace]
  (into {} (map (fn [entry]
                  [(:choice-id entry) (:value entry)])
                trace)))

;;; Inference

(defmethod checkpoint [::algorithm embang.trap.sample] [_ smp]
  (let [[choice-id state] (choice-id smp (:state smp))
        value (if (contains? (state ::rdb) choice-id)
                ((state ::rdb) choice-id)
                (sample (:dist smp)))
        log-p (try
                (observe (:dist smp) value)
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
        state (-> state
                  (add-log-weight log-p)
                  (record-choice choice-id value cont))]
    #((:cont smp) value state)))

;;; State transition

(defn next-state
  "produces next state given current state
  and the trace entry to resample"
  [state entry]
  (:state (exec ::algorithm (:cont entry) nil 
                ;; Remove the selected entry from RDB.
                {::rdb (dissoc (rdb (state ::trace))
                               (:choice-id entry))})))

;;; Reporting the mode

(defn add-trace-predict
  "adds trace as a predict"
  [state]
  (add-predict state
               '$trace (map :value (::trace state))))

(defmethod infer :siman [_ prog & {:keys [cooling-rate
                                          number-of-samples]
                                   :or {cooling-rate 0.99}}]
  ;; The MAP inference consists of two chained transformations,
  ;; `sample-seq', followed by `map-seq'.
  (letfn
    [(sample-seq [state T]
       ;; Produces samples via simulated annealing.
       (lazy-seq
         (if (seq (state ::trace))
           (let [;; Choose uniformly a random choice to resample.
                 entry (rand-nth (state ::trace))
                 ;; Compute next state from the resampled choice.
                 next-state (next-state state entry)
                 state (if (> (/ (- (get-log-weight next-state)
                                    (get-log-weight state))
                                 T)
                              (Math/log (rand)))
                         next-state
                         state)]
             (cons (add-trace-predict state)
                  (sample-seq state (* T cooling-rate))))

           ;; Deterministic program, the only output is the mode.
           (list (add-trace-predict state)))))

     (map-seq [sample-seq max-log-weight]
       ;; Filters MAP estimates by increasing weight.
       (lazy-seq
         (when-let [[map & sample-seq] sample-seq]
           (if (> (get-log-weight map) max-log-weight)
             (cons map (map-seq sample-seq (get-log-weight map)))
             (map-seq sample-seq max-log-weight)))))]

    (let [sample-seq (sample-seq
                       (:state (exec ::algorithm
                                     prog nil initial-state)) 1.)
          sample-seq (if number-of-samples
                       (take number-of-samples sample-seq)
                       sample-seq)]
      (map-seq sample-seq (Math/log 0.)))))
