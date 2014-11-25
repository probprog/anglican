(ns embang.smc
  (:use embang.state
        embang.inference
        [embang.runtime :only [observe]]))

;;; SMC

(derive ::algorithm :embang.inference/algorithm)

(defmethod checkpoint [::algorithm embang.trap.observe] [algorithm obs]
  ;; update the weight and return the observation checkpoint
  ;; for possible resampling
  obs
  (update-in obs [:state]
             add-log-weight (observe (:dist obs) (:value obs))))

(declare resample)

(defn smc-sweep
  "a single SMC sweep, can be called from other algorithms"
  [algorithm prog number-of-particles]
  (loop [particles (repeatedly number-of-particles
                               #(exec algorithm prog nil initial-state))]
    (cond
     (every? #(instance? embang.trap.observe %) particles)
     (recur (map #(exec algorithm (:cont %) nil (:state %))
                 (resample particles)))

     (every? #(instance? embang.trap.result %) particles)
     particles
     
     :else (throw (AssertionError.
                   "some `observe' directives are not global")))))

;; SMC produces all of its samples in parallel,
;; therefore the default number of samples cannot
;; be infinity. We arbitrarily set it to 1.

(defmethod infer :smc [_ prog & {:keys [number-of-samples
                                        number-of-sweeps
                                        number-of-particles
                                        output-format]
                                 :or {number-of-samples 1
                                      output-format :clojure}}]
  (let [number-of-sweeps (or number-of-sweeps
                             (when number-of-samples 1))
        number-of-particles (or number-of-particles
                                number-of-samples)]
    (dotimes [_ number-of-sweeps]
      (doseq [res (smc-sweep ::algorithm prog number-of-particles)]
        (print-predicts (:state res) output-format)))))

(defn resample
  "resamples particles proportionally to their current weights"
  [particles]
  (let [log-weights (map (comp get-log-weight :state) particles)
        max-log-weight (max log-weights)
        weights (map #(Math/exp (- % max-log-weight)) log-weights)
        total-weight (reduce + weights)
        step (/ total-weight (count particles))]
    (loop [x (rand total-weight)
           old-weights weights
           old-particles particles
           new-particles []]
      particles)))

        
