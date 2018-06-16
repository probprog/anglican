(ns anglican.pimh
  "Particle Independent Metropolis-Hastings
   Options:
     :number-of-particles (2 by default)
       - number of particles per sweep"
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:use [anglican.state :only [initial-state add-log-weight
                               set-log-weight]]
        #?(:clj anglican.inference
          :cljs [anglican.inference :only [infer exec checkpoint]])
        [anglican.lmh :only [accept?]]
        [anglican.runtime :only [observe*]]
        [anglican.smc :only [resample particle-weights sweep]]))

;;; Particle Independent Metropolis-Hastings (PIMH)

(derive ::algorithm :anglican.smc/algorithm)

(defmethod checkpoint [::algorithm anglican.trap.observe] [_ obs]
  ;; update the weight and return the observation checkpoint
  ;; for possible resampling
  (update-in obs [:state]
             add-log-weight (observe* (:dist obs) (:value obs))))

(defmethod sweep ::algorithm
  [algorithm prog value number-of-particles]
  (loop [particles (repeatedly number-of-particles
                               #(exec algorithm
                                      prog value initial-state))]
    (cond
     (every? #(instance? anglican.trap.observe %) particles)
     (recur (map #(exec algorithm (:cont %) nil (:state %))
                 (resample particles number-of-particles)))

     (every? #(instance? anglican.trap.result %) particles)
     particles

     :else (throw (ex-info "some `observe' directives are not global"
                           {:particles particles})))))

(defmethod infer :pimh [_ prog value
                        & {:keys [number-of-particles]   ; per sweep
                           :or {number-of-particles 1}}]
  (assert (>= number-of-particles 1)
          ":number-of-particles must be at least 1")
  (letfn
    [;; Compute normalization factor, required for MH.
     (get-log-Z [particles]
       (let [[weights max-log-weight] (particle-weights particles)]
         (+ (Math/log (reduce + weights)) max-log-weight)))

     ;; Add samples produced by the particles to the sample sequence.
     (add-samples [particles log-Z]
       (concat (map (comp #(set-log-weight % 0.) :state) particles)
               (sample-seq particles log-Z)))

     (sample-seq [particles log-Z]
       (lazy-seq
         ;; Run a new sweep.
         (let [next-particles (sweep ::algorithm
                                     prog value number-of-particles)
               next-log-Z (get-log-Z next-particles)]
           ;; And accept with MH probability.
           (if (accept? next-log-Z log-Z)
             (add-samples next-particles next-log-Z)
             (add-samples particles log-Z)))))]

    ;; Run the first sweep to initialize the process.
    (let [particles (sweep ::algorithm
                           prog value number-of-particles)]
      (sample-seq particles (get-log-Z particles)))))
