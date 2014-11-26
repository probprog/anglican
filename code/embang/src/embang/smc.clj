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

(declare resample)

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

;;; Resampling particles

;; Systematic resampling is used. The particles are assigned
;; unit weight after resampling.

(defn resample
  "resamples particles proportionally to their current weights"
  ([particles] (resample particles (count particles)))
  ([particles number-of-new-particles]
   (let [log-weights (map (comp get-log-weight :state) particles)
         max-log-weight (reduce max log-weights)
         weights (map #(Math/exp (- % max-log-weight)) log-weights)
         total-weight (reduce + weights)]

     (if (= total-weight 0.) particles   ; all particles have
       ; the same weight
       ;;; Systematic sampling

       ;; invariant bindings for sampling
       (let [step (/ total-weight number-of-new-particles)
             all-weights weights     ; particles are circular
             all-particles particles]

         (loop [x (rand total-weight)
                n 0      ; number of particles sampled so far
                acc 0    ; upper bound of the current segment
                weights weights
                particles particles
                new-particles nil]
           (if (= n number-of-new-particles)
             new-particles
             (let [[weight & next-weights] weights
                   [particle & next-particles] particles
                   next-acc (+ acc weight)]
               (if (< x next-acc)

                 ;; Found the wheel segment into which x has fallen.
                 ;; Advance x by step for the next particle's segment.
                 (recur (+ x step) (+ n 1) 
                        acc weights particles
                        (conj new-particles
                              (update-in
                                particle [:state]
                                set-log-weight 0.)))

                 ;; Otherwise, keep going through the particle's 
                 ;; segments, recycling the list of particles and
                 ;; their weights when necessary.
                 (recur x n
                        next-acc
                        (or next-weights all-weights)
                        (or next-particles all-particles)
                        new-particles))))))))))
