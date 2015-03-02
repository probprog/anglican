(ns embang.pimh
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:use embang.state
        embang.inference
        [embang.runtime :only [observe]]
        embang.smc))

;;; Particle Independent Metropolis-Hastings (PIMH)

(derive ::algorithm :embang.smc/algorithm)

(defmethod checkpoint [::algorithm embang.trap.observe] [_ obs]
  ;; update the weight and return the observation checkpoint
  ;; for possible resampling
  (update-in obs [:state]
             add-log-weight (observe (:dist obs) (:value obs))))

(defmethod sweep ::algorithm
  [algorithm prog value number-of-particles]
  (loop [particles (repeatedly number-of-particles
                               #(exec algorithm
                                      prog value initial-state))
         log-Z 0.]
    (cond
     (every? #(instance? embang.trap.observe %) particles)
     (recur (map #(exec algorithm (:cont %) nil (:state %))
                 (resample particles number-of-particles))
            (- (+ log-Z (Math/log
                          (reduce
                            + (recover-weights
                                (map (comp get-log-weight :state)
                                     particles)))))
               (Math/log number-of-particles)))

     (every? #(instance? embang.trap.result %) particles)
     [particles log-Z]

     :else (throw (AssertionError.
                   "some `observe' directives are not global")))))

(defmethod infer :pimh [_ prog value & {:keys [number-of-particles]
                                 :or {number-of-particles 1}}]
  (assert (>= number-of-particles 1)
          ":number-of-particles must be at least 1")
  (letfn [(add-particles [particles log-Z]
            (concat (map :state particles)
                    (sample-seq particles log-Z)))
          (sample-seq [particles log-Z]
            (lazy-seq
              ;; Run a new sweep.
              (let [[new-particles new-log-Z]
                    (sweep ::algorithm prog value number-of-particles)]
                ;; And accept with MH probability.
                (if (> (- new-log-Z log-Z) (Math/log (rand)))
                  (add-particles new-particles new-log-Z)
                  (add-particles particles log-Z)))))]
    ;; Run the first sweep to initialize the process.
    (let [[particles log-Z]
          (sweep ::algorithm prog value number-of-particles)]
      (sample-seq particles log-Z))))
