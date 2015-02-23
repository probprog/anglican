(ns embang.pfilter
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:use embang.state
        embang.inference
        embang.smc))

;;; Particle Filter 
;;
;; Particle filter is NOT a probabilistic programming inference
;; algorithm. Every observe creates a new batch of output states,
;; the program may run infinitely.

(derive ::algorithm :embang.smc/algorithm)

(defmethod infer :pfilter [_ prog value & {:keys [number-of-particles]
                                           :or {number-of-particles 1}}]
  (assert (>= number-of-particles 1)
          ":number-of-particles must be at least 1")
  (let [initial-particles
        (repeatedly number-of-particles
                    #(exec ::algorithm prog value initial-state))]
    (letfn
      [(sample-seq [particles]
         (lazy-seq
             (cond
               (every? #(instance? embang.trap.observe %) particles)
               (concat
                 (keep
                   (fn [particle]
                     (let [state (:state particle)]
                       ;; Skip states without predicts.
                       (when (seq (get-predicts state))
                         ;; The state is after `observe' and before
                         ;; resampling, restore the weight to 1.
                         (set-log-weight state 0))))
                   particles)
                 (sample-seq 
                   (map #(exec ::algorithm (:cont %) nil 
                               (clear-predicts (:state %)))
                        (resample particles))))

               (every? #(instance? embang.trap.result %) particles)
               (concat 
                 (map :state particles)
                 (sample-seq initial-particles))

               :else (do
                       (throw (AssertionError.
                                (str "some `observe' directives "
                                     "are not global")))))))]
      (sample-seq initial-particles))))
