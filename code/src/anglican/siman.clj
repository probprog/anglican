(ns anglican.siman
  "MAP estimation via Simulated Annealing
   Options:
     :cooling-rate (0.99 by default)
        - cooling rate, should less than 1
     :cooling-schedule (:exponential by default)
        - cooling schedule, :exponential or :lundy-mees
     :predict-candidates (false by default)
        - output all samples rather than those with increasing
          log-weight"
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:use [anglican.state :exclude [initial-state]]
        anglican.inference
        [anglican.runtime :only [observe sample]]))

;;;; Finding MAP of the trace using simulated annealing

(derive ::algorithm :anglican.inference/algorithm)

;;; Initial state

(def initial-state
  "initial state for PGibbs protocol"
  (into anglican.state/initial-state
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
  "returns a unique idenditifer for sample checkpoint
  and the updated state"
  [smp state]
  (checkpoint-id smp state ::choice-counts ::choice-last-id))

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

(defmethod checkpoint [::algorithm anglican.trap.sample] [_ smp]
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

(defmulti next-temperature
  "returns decreased temperature"
  (fn [schedule temperature rate] schedule))

(defmethod next-temperature :exponential
  ;; T'=bT, mostly works.
  [_ temperature rate]
  (* temperature rate))

(defmethod next-temperature :lundy-mees
  ;; T' = T/(1+(1-b)T), see [M Lundy and A Mees. 1986. Convergence of
  ;; an annealing algorithm. Math. Program. 34, 1 (January 1986),
  ;; 111-124] for details.
  [_ temperature rate]
  (/ temperature (+ 1. (* (- 1. rate) temperature))))

(defmethod infer :siman
  [_ prog value
   & {:keys [;; A real number slightly smaller than 1.,
             ;; the closer to 1., the slower the convergence.
             cooling-rate
             ;; The algorithm to compute subsequent temperatures,
             ;; currently, :exponential or :lundy-mees.
             cooling-schedule
             ;; Add the trace as a predict.
             predict-trace
             ;; Output all states rather than just states
             ;; with increasing log-weight.
             predict-candidates
             ;; Total number of samples to produce.
             number-of-samples]
      :or {cooling-rate 0.99
           cooling-schedule :exponential
           predict-trace false
           predict-candidates false}}]
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
                         state)
                 state (if predict-trace
                         (add-trace-predict state)
                         state)]
             (cons state
                   (sample-seq
                     state (next-temperature cooling-schedule
                                             T cooling-rate))))

           ;; Deterministic program, the only output is the mode.
           (repeat (add-trace-predict state)))))

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
                       (:state (exec ::algorithm
                                     prog value initial-state)) 1.)
          sample-seq (if number-of-samples
                       (take number-of-samples sample-seq)
                       sample-seq)]
      (map-seq sample-seq (Math/log 0.)))))
