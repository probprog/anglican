(ns anglican.pfilter
  "Particle Filter
   Options:
     :number-of-particles (1 by default)
       - number of particles per sweep"
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:use anglican.state
        anglican.inference
        anglican.smc))

;;; Particle Filter 
;;
;; Particle filter is NOT a probabilistic programming inference
;; algorithm. Every `observe' creates a new batch of output states,
;; the program may run infinitely.

(derive ::algorithm :anglican.smc/algorithm)

(defmethod infer :pfilter [_ prog value
                           & {:keys [number-of-particles]  ; per sweep
                              :or {number-of-particles 1}}]
  (assert (>= number-of-particles 1)
          ":number-of-particles must be at least 1")
  (let [initial-particles
        (repeatedly number-of-particles
                    #(exec ::algorithm prog value initial-state))]
    (letfn
      [(sample-seq [particles]
         (lazy-seq
           (concat
             ;; Keep only states which contain predicts.
             (keep
               (fn [particle]
                 (let [state (:state particle)]
                   ;; Skip states without predicts.
                   (when (seq (get-predicts state))
                     ;; The state is after `observe' and before
                     ;; resampling, restore the weight to 1, so
                     ;; that all predicts have the same weight.
                     (set-log-weight state 0))))
               particles)
             
             ;; Continue running the program infinitely.
             (sample-seq 
               (cond
                 (every? #(instance? anglican.trap.observe %) particles)
                 ;; Resample and continue.
                 (map #(exec ::algorithm (:cont %) nil 
                             (clear-predicts (:state %)))
                      (resample particles number-of-particles))

                 (every? #(instance? anglican.trap.result %) particles)
                 ;; Restart the particles.
                 initial-particles

                 :else (do
                         (throw (AssertionError.
                                  (str "some `observe' directives "
                                       "are not global")))))))))]

      (sample-seq initial-particles))))
