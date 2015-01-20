(ns embang.siman
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
        {::trace []       ; current random choices
         ::rdb {}         ; stored random choices
         ::counts {}      ; counts of occurences of each `sample'
         ::last-id nil})) ; last sample id

;;; Trace

;; The trace is a vector of entries
;;   {choice-id value mk-cont}
;; where
;;   - `choice-id' is the identifier of the random choice
;;   - `value' is the value of random choice in the current
;;     run,
;;   - `mk-cont' is the continuation constructor that
;;     accepts a new database and returns the continuation
;;     that starts at the checkpoint.

(defrecord entry [choice-id value mk-cont])

(defn record-random-choice
  "records random choice in the state"
  [state choice-id value mk-cont]
  (let [sample-id (first choice-id)]
    (-> state
        (update-in [::trace]
                   conj (->entry choice-id value mk-cont))
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

(defn mk-rdb
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
        log-p (try
                (observe (:dist smp) value)
                ;; NaN is returned if value is not in support.
                (catch Exception e (/ 0. 0.)))
        value (if (< (/ -1. 0.) log-p (/ 1. 0.)) value
                ;; The retained value is not in support,
                ;; resample the value from the prior.
                (sample (:dist smp)))
        mk-cont (fn [rdb]
                  (fn [_ state]
                    (assoc-in smp [:state ::rdb] rdb)))
        state (-> state
                  (add-log-weight log-p)
                  (record-random-choice choice-id value mk-cont))]
    #((:cont smp) value state)))

;;; State transition

(defn mk-next-state
  "produces next state given current state
  and the trace entry to resample"
  [state entry]
  (let [rdb (dissoc (mk-rdb (state ::trace)) (:choice-id entry))
        prog ((:mk-cont entry) rdb)]
    (:state (exec ::algorithm prog nil initial-state))))

(defmethod infer :siman [_ prog & {:keys [cooling-rate
                                          number-of-samples]
                                   :or {cooling-rate 0.99}}]
  ;; The MAP inference consists of two chained transformations,
  ;; `sample-seq', followed by `map-seq'.
  (letfn
    [(sample-seq [state T]
       ;; Produces samples via simulated annealing.
       (lazy-seq
         (let [entry (rand-nth (state ::trace))
               next-state (mk-next-state state entry)
               state (if (> (/ (- (get-log-weight next-state)
                                  (get-log-weight state))
                               T)
                            (Math/log (rand)))
                       next-state
                       state)]
           
             (cons (add-predict state
                                '$trace (map :value (::trace state)))
                   (sample-seq state (* T cooling-rate))))))

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
