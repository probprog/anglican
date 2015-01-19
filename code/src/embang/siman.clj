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
;;   {choice-id value log-p mk-cont}
;; where
;;   - `choice-id' is the identifier of the random choice
;;   - `value' is the value of random choice in the current
;;     run,
;;   - `log-p' is the log probability (mass or density) of
;;     the value given the distribution,
;;   - `mk-cont' is the continuation constructor that
;;     accepts a new database and returns the continuation
;;     that starts at the checkpoint.

(defrecord entry [choice-id value log-p mk-cont])

(defn record-random-choice
  "records random choice in the state"
  [state choice-id value log-p mk-cont]
  (let [sample-id (first choice-id)]
    (-> state
        (update-in [::trace]
                   conj (->entry choice-id value log-p mk-cont))
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
        log-p (observe (:dist smp) value)
        value (if-not (Double/isFinite log-p) 
                ;; The retained value is not in support,
                ;; resample the value from the prior.
                (sample (:dist smp))
                value)
        mk-cont (fn [rdb]
                  (fn [_ state]
                    (assoc-in smp [:state ::rdb] rdb)))
        state (record-random-choice state
                                    choice-id value log-p mk-cont)]
    #((:cont smp) value state)))

(defn energy
  "computes state energy, used to determine
  the acceptance log-probability as (next-energy - prev-energy)/T"
  [state]
  (reduce + (get-log-weight state)
          (map :log-p (state ::trace))))

(defmethod infer :siman [_ prog & {:keys [cooling-rate]
                                   :or {cooling-rate 1}}]
  (letfn
    [(sample-seq [state T]
       (let [entry (rand-nth (state ::trace))
             entry-id (:choice-id entry)

             next-rdb (dissoc (mk-rdb (state ::trace)) entry-id)
             next-prog ((:mk-cont entry) next-rdb)
             next-state (:state (exec ::algorithm
                                      next-prog nil initial-state))

             state (if (> (/ (- (energy next-state) (energy state))
                             (+ 1. (* cooling-rate T)))
                          (Math/log (rand)))
                     next-state
                     state)]
         (lazy-seq
           (cons (set-log-weight state (energy state))
                 (sample-seq state (inc T))))))]
    (sample-seq
      (:state (exec ::algorithm prog nil initial-state)) 0)))
