(ns embang.pgibbs
  (:use embang.state
        [embang.inference :exclude [initial-state]]
        [embang.runtime :only [observe]]))

;;;; PGibbs

(derive ::algorithm :embang.smc/algorithm)

;;; Initial state

(def initial-state
  "initial state for PGibbs protocol"
  (into embang.inference/initial-state
        ;; the state is extended by two sequences,
        ;;   ::future-samples used by retained particle
        ;;   ::past-samples updated by all particles
        {::future-samples []
         ::past-samples []}))

;;; Retained state operations

(defn retained-initial-state
  "returns the initial state for a retained particle"
  [state]
  (assoc initial-state
         ::future-samples (::past-samples state)))

(defn retained-state?
  "whether the state is retained"
  [state]
  (seq (::future-samples state)))

(defn retrieve-retained-sample
  "returns the updated state with the rest of samples
  and the retained sample"
  [state]
  [(update-in state [::future-samples] rest) 
   (first (::future-samples state))])

(defn forget-retained-samples
  "returns the updated state with future samples removed"
  [state]
  (if (retained-state? state)
    (assoc state ::future-samples [])
    state))
    
(defn store-sample
  "returns the updated state with the new sample
  added to past-samples"
  [state value]
  (update-in state [::past-samples] conj value))

(defmethod checkpoint [::algorithm embang.trap.observe] [algorithm obs]
  ;; update the weight and return the observation checkpoint
  ;; for possible resampling
  obs
  (update-in obs [:state]
             add-log-weight (observe (:dist obs) (:value obs))))

(defmethod checkpoint [::algorithm embang.trap.sample] [algorithm smp]
  (let [state (:state smp)
        [state value] (if (retained-state? state)
                        (retrieve-retained-sample state)
                        [state (sample (:dist smp))])
        state (store-sample state value)]
  #((:cont smp) (sample (:dist smp)) (:state smp)))

(declare resample)

(defn pgibbs-sweep
  "a single PGibbs sweep, can be called from other algorithms"
  [algorithm prog number-of-particles]
  (loop [particles (repeatedly number-of-particles
                               #(exec algorithm prog nil initial-state))]
    (cond
     (every? #(instance? embang.trap.observe %) particles)
     (recur (map #(exec algorithm (:cont %) nil (:state %))
                 (resample particles)))

     (every? #(instance? embang.trap.result %) particles)
     (resample particles)
     
     :else (throw (AssertionError.
                   "some `observe' directives are not global")))))

(defmethod infer :pgibbs [_ prog & {:keys [number-of-sweeps
                                        number-of-particles
                                        output-format]
                                 :or {number-of-particles 2
                                      output-format :clojure}}]
  (loop [i 0]
    (when-not (= i number-of-sweeps)
      (doseq [res (pgibbs-sweep ::algorithm prog number-of-particles)]
        (print-predicts (:state res) output-format))
      (recur (inc i)))))
