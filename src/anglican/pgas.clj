(ns anglican.pgas
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:require [anglican.smc :as smc :refer [sweep]]
            [anglican.runtime :refer [sample* observe* discrete exp]])
  (:use [anglican.state :exclude [initial-state]]
        anglican.inference))

;;;; Particle Gibbs with Ancestor Sampling
;;;; (with LMH-style rescoring)

(derive ::algorithm :anglican.inference/algorithm)
(derive ::rescore :anglican.inference/algorithm)

;;; Initial State

(def initial-state
  "initial state for PGAS"
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

(defn get-trace "returns trace" [state] (state ::trace))

;; The trace is a vector of entries
;;   {choice-id value log-p sampled?}
;; where
;;   - `choice-id' is the identifier of the random choice,
;;   - `value' is the value of random choice in the current run,
;;   - `log-p' is the log probability (mass or density) of
;;     the value given the distribution,
;;   - `sampled?' indicates whether the entry was sampled (or rescored)

(defrecord entry [choice-id value log-p sampled?])

(defn choice-id
  "returns a unique idenditifer for sample checkpoint
  and the updated state"
  [smp state]
  (checkpoint-id smp state ::choice-counts ::choice-last-id))

(defn rdb
  "creates random database from trace"
  [trace]
  (into {} (map (fn [entry]
                  [(:choice-id entry) (:value entry)])
                trace)))

(defmethod checkpoint [::algorithm anglican.trap.observe] [_ obs]
  (let [state (add-log-weight (:state obs)
                              (observe* (:dist obs) (:value obs)))]
    ;; return checkpoint to inference algorithm
    (assoc obs :state state)))

(defn next-entry
  "constructs a new trace entry for a sample checkpoint by rescoring
  the rdb value when possible and sampling from the prior when not"
  [smp]
  (let [[choice-id state] (choice-id smp (:state smp))
        id-in-rdb? (contains? (state ::rdb) choice-id)
        value (when id-in-rdb?
                ((state ::rdb) choice-id))
        log-p (try (observe* (:dist smp) value)
                (catch Exception e nil))
        entry (if (and id-in-rdb? log-p)
                (->entry choice-id value log-p false)
                (let [value (sample* (:dist smp))
                      log-p (observe* (:dist smp) value)]
                  (->entry choice-id value log-p true)))
        state (update-in state
                         [::trace] conj entry)]
    [entry state]))

(defmethod checkpoint [::algorithm anglican.trap.sample] [_ smp]
  (let [[entry state] (next-entry smp)]
    #((:cont smp) (:value entry) state)))

(defmethod checkpoint [::rescore anglican.trap.sample] [_ smp]
  (let [[entry state] (next-entry smp)]
    ;; add log probability of sample to log weight
    ;; when the sample was rescored
    (if (:sampled? entry)
      #((:cont smp) (:value entry) state)
      #((:cont smp) (:value entry) (add-log-weight state
                                                   (:log-p entry))))))

(defn retained-entries [state]
  (filter
   (comp not :sampled?)
   (state ::trace)))

(defn get-log-rescored-probability [state]
  (reduce + 0.0 (map :log-p
                     (retained-entries state))))

(defn resample-retained
  "resamples the retained particle by rescoring
  all particles relative to the retained db"
  ([particles retained-particle]
   (let [;; rescore particles by executing continuation
         retained-db (get-in retained-particle [:state ::rdb])
         all-particles (conj particles retained-particle)
         regenerated (mapv #(exec ::rescore
                                  (:cont %)
                                  nil
                                  (assoc (:state %)
                                    ::rdb retained-db))
                           all-particles)
         ;; sample ancestor index
         log-weights (map (comp get-log-weight :state)
                          regenerated)
         max-log-weight (reduce max log-weights)
         weights (map #(exp (- % max-log-weight)) log-weights)
         ancestor (sample* (discrete weights))]
     ;; return new retained particle with regenerated db
     (assoc-in (nth all-particles ancestor)
               [:state ::rdb]
               (rdb (get-in (nth regenerated ancestor)
                            [:state ::trace]))))))

(defmethod sweep ::algorithm
  [algorithm prog value number-of-particles retained-db]
  (loop [particles (repeatedly (- number-of-particles 1)
                               #(exec algorithm
                                      prog
                                      value
                                      initial-state))
         retained-particle (exec algorithm
                                 prog
                                 value
                                 (assoc initial-state
                                   ::rdb retained-db))]
    (cond
     (every? #(instance? anglican.trap.observe %) particles)
     (recur
      ;; for normal particles, perform conditional resample
      ;; and execute to next checkpoint
      (map #(exec algorithm (:cont %) nil
                  (-> (:state %)
                      ;; log weights after resampling are 0.0 in pgas
                      (set-log-weight 0.)
                      ;; ensure that rdb is empty for descendants
                      ;; of the retained particle
                      (assoc ::rdb {})))
           (smc/resample (conj particles retained-particle)
                         (dec number-of-particles)))
      ;; resample ancestor for retained particle,
      ;; and execute until next checkpoint
      (-> (resample-retained particles retained-particle)
          (#(exec algorithm (:cont %) nil
                  (-> (:state %)
                      ;; log weight after resampling is 0.0 in pgas
                      (set-log-weight 0.))))))
     (every? #(instance? anglican.trap.result %) particles)
     (conj particles retained-particle)
     :else (throw (AssertionError.
                   "some `observe' directives are not global")))))

(defmethod infer :pgas [_ prog value
                        & {:keys [number-of-particles]
                           :or {number-of-particles 2}}]
  (assert (>= number-of-particles 2)
          ":number-of-particles must be at least 2")
  (letfn [(sample-seq [retained-db]
            (lazy-seq
              (let [particles (sweep ::algorithm
                                prog value
                                number-of-particles retained-db)]
                (concat (map :state particles)
                        (sample-seq (rdb (get-in (rand-nth particles)
                                            [:state ::trace])))))))]
    (sample-seq {})))
