(ns embang.lmh
  (:use [embang.state :exclude [initial-state]]
        embang.inference
        [embang.runtime :only [observe sample]]))

;;;; Lightweight (single-site) Metropolis-Hastings

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

(defn utility
  "computes state utility, used to determine
  the acceptance log-probability as (next-utility - prev-utility)"
  [state]
  (+ (get-log-weight state)
     (reduce + (keep
                 (fn [{:keys [choice-id value log-p]}]
                   (when (and (contains?  (state ::rdb) choice-id)
                              ;; The value was present in the
                              ;; database, but still resampled.
                              (= value ((state ::rdb) choice-id)))
                     log-p))
                 (state ::trace)))
     (- (Math/log (count (state ::trace))))))

(defmethod infer :lmh [_ prog & {}]
  (letfn
    [(sample-seq [state]
       (let [entry (rand-nth (state ::trace))
             entry-id (:choice-id entry)

             next-rdb (dissoc (mk-rdb (state ::trace)) entry-id)
             next-prog ((:mk-cont entry) next-rdb)
             next-state (:state (exec ::algorithm
                                      next-prog nil initial-state))

             ;; Previous state is the state that would be obtained
             ;; by transitioning from the new state to the current
             ;; state.
             prev-rdb (dissoc (mk-rdb (next-state ::trace)) entry-id)
             prev-state (assoc state ::rdb prev-rdb)

             state (if (> (- (utility next-state) (utility prev-state))
                          (Math/log (rand)))
                     next-state
                     state)]
         (lazy-seq
           (cons (set-log-weight state 0.) (sample-seq state)))))]
    (sample-seq (:state (exec ::algorithm prog nil initial-state)))))
