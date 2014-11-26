(ns embang.smc
  (:use embang.state
        embang.inference
        [embang.runtime :only [observe]]))

;;; SMC

(derive ::algorithm :embang.inference/algorithm)

(defmethod checkpoint [::algorithm embang.trap.observe] [algorithm obs]
  ;; update the weight and return the observation checkpoint
  ;; for possible resampling
  (update-in obs [:state]
             add-log-weight (observe (:dist obs) (:value obs))))

(defn smc-sweep
  "a single SMC sweep"
  [prog number-of-particles]
  (loop [particles (repeatedly number-of-particles
                               #(exec ::algorithm prog nil initial-state))]
    (cond
     (every? #(instance? embang.trap.observe %) particles)
     (recur (map #(exec ::algorithm (:cont %) nil (:state %))
                 (resample particles)))

     (every? #(instance? embang.trap.result %) particles)
     particles
     
     :else (throw (AssertionError.
                   "some `observe' directives are not global")))))

(defmethod infer :smc [_ prog & {:keys [number-of-sweeps
                                        number-of-particles
                                        output-format]
                                 :or {number-of-particles 2
                                      output-format :clojure}}]
  (loop [i 0]
    (when-not (= i number-of-sweeps)
      (doseq [res (smc-sweep prog number-of-particles)]
        (print-predicts (:state res) output-format))
      (recur (inc i)))))
