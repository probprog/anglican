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
  (let [number-of-particles (count particles)
        log-weights (map (comp get-log-weight :state) particles)
        max-log-weight (reduce max log-weights)
        weights (map #(Math/exp (- % max-log-weight)) log-weights)
        total-weight (reduce + weights)

        ;;; Systematic sampling

        ;; invariant bindings for sampling
        step (/ total-weight (count particles))
        all-weights weights     ; weights and particles are circular
        all-particles particles]

    (loop [x (rand total-weight)
           n 0
           acc 0
           weights weights
           particles particles
           new-particles nil]
      (if (= n number-of-particles)
        new-particles
        (let [[weight & next-weights] weights
              [particle & next-particles] particles
              next-acc (+ acc weight)]
          (if (> acc x)
              (recur (+ x step) (+ n 1) 
                     acc weights particles
                     (conj new-particles
                           (update-in particle [:state]
                                      set-log-weight 0.)))
              (recur x n
                     next-acc
                     (or next-weights all-weights)
                     (or next-particles all-particles)
                     new-particles)))))))
