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

;; SMC produces all of its samples in parallel,
;; therefore the default number of samples cannot
;; be infinity. We arbitrarily set it to 1.

(declare resample)

;; TODO: smc-sweep, smc calls smc-sweep, smc-sweep parameterized by algorithm

(defmethod infer :smc [_ prog & {:keys [number-of-samples output-format]
                                 :or {number-of-samples 1 
                                      output-format :clojure}}]

  (loop [particles (repeatedly number-of-samples #(exec ::algorithm prog nil initial-state))]
    (cond
      (every? #(instance? embang.trap.observe %) particles)
      (recur (map #(exec ::algorithm (:cont %) nil (:state %))
                  (resample particles)))

      (every? #(instance? embang.trap.result %) particles)
      (doseq [res (resample particles)]
        (do (print-predicts (:state res) output-format)
            particles))

      :else (throw (AssertionError. "some `observe' directives are not global")))))

(defn resample
  "resamples particles proportionally to their current weights"
  [particles]
  particles)
