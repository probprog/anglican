(ns embang.pgibbs
  "Particles Gibbs (Iterated Conditional SMC)
   Options:
     :number-of-particles (2 by default)
       - number of particles per sweep"
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:use [embang.state :exclude [initial-state]]
        embang.inference
        embang.smc
        [embang.runtime :only [observe sample]]))

;;;; Particle Gibbs

(derive ::algorithm :embang.smc/algorithm)

;;; Initial state

(def initial-state
  "initial state for Particle Gibbs"
  (into embang.state/initial-state
        ;; the state is extended by two sequences,
        ;;   ::future-samples used by retained particle
        ;;   ::past-samples updated by all particles
        {::future-samples []
         ::past-samples []}))

;;; Retained state operations

(defn retained-initial-state
  "returns the initial state for a retained particle"
  [retained-particle]
  (assoc initial-state
         ::future-samples ((:state retained-particle) ::past-samples)))

(defn retained-state?
  "whether the state is retained"
  [state]
  (seq (state ::future-samples)))

(defn retrieve-retained-sample
  "returns the updated state with the rest of samples
  and the retained sample"
  [state]
  [(update-in state [::future-samples] rest) 
   (first (state ::future-samples))])

(defn release-retained-state
  "returns the updated state with ::future-samples removed,
  so that next random choices are sampled rather than restored"
  [state]
  (if (retained-state? state)
    (assoc state ::future-samples [])
    state))
    
(defn store-sample
  "returns the updated state with the new sample
  added to past-samples"
  [state value]
  (update-in state [::past-samples] conj value))

;; sample checkpoint for pgibbs --- sample the value,
;; except for retained particle, and store in past-particles

(defmethod checkpoint [::algorithm embang.trap.sample] [_ smp]
  (let [state (:state smp)
        [state value] (if (retained-state? state)
                        (retrieve-retained-sample state)
                        [state (sample (:dist smp))])
        state (store-sample state value)]
    #((:cont smp) value state)))

;;; Inference loop

(defmethod sweep ::algorithm
  [algorithm prog value number-of-particles retained-state]
  (loop [particles 
         (conj
          (repeatedly (- number-of-particles 1)
                      #(exec algorithm prog value initial-state))
          (exec algorithm prog value retained-state))]
    (cond
     (every? #(instance? embang.trap.observe %) particles)
     (recur (map #(exec algorithm (:cont %) nil
                        ;; Set weights of all particles (including
                        ;; the retained one) to the same value.
                        (set-log-weight (:state %) 0.))
                 (conj 
                  ;; Resample all but one from all particles
                  ;; including the retained one, but release the
                  ;; retained state so that the choices in resampled
                  ;; particles are drawn rather than recovered.
                  (resample 
                   (conj (rest particles)
                         (update-in (first particles) [:state]
                                    release-retained-state))
                   (- number-of-particles 1))

                  ;; Add the retained particle at the first position.
                  (first particles))))
                       
     (every? #(instance? embang.trap.result %) particles)
     particles
     
     :else (throw (AssertionError.
                   "some `observe' directives are not global")))))

(defmethod infer :pgibbs [_ prog value
                          & {:keys [number-of-particles]   ; per sweep
                             :or {number-of-particles 2}}]
  (assert (>= number-of-particles 2)
          ":number-of-particles must be at least 2")
  (letfn [(sample-seq [retained-state]
            (lazy-seq
              (let [particles (sweep ::algorithm
                                prog value number-of-particles
                                retained-state)
                    retained-state (retained-initial-state
                                     (rand-nth particles))]
                (concat (map :state particles)
                        (sample-seq retained-state)))))]
    (sample-seq initial-state)))
