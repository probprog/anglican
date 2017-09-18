(ns anglican.ais
  "Annealed Importance Sampling
   Options:
     :number-of-steps (1 by default)
        - number of interpolation steps to perform
     :start-exponent (0 by default)
        - initial exponent in the cooling schedule
     :end-exponent (1 by default)
        - final exponent in the cooling schedule"
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:use [anglican.state :exclude [initial-state]]
        anglican.inference
        [anglican.rmh :exclude [utility]]))

;;;; Annealed Importance Sampling

(derive ::algorithm :anglican.rmh/algorithm)

(defn linspace
  "returns a equally spaced sequence of points"
  [start end size]
  (let [delta (/ (- end start) (dec size))]
    (map (fn [n] (+ start (* n delta)))
         (range size))))

(defn utility
  "computes state utility, used to determine
  the acceptance log-probability as (next-utility - prev-utility)"
  [state exponent]
  (+ (* (get-log-weight state) exponent)
     (get-log-retained-probability state)
     (- (Math/log (count (get-trace state))))))

(defn anneal
  "performs a single annealed importance sampling sweep"
  [prog value exponents]
  (loop [exponents exponents
         state (:state (exec ::algorithm prog value initial-state))
         log-weight 0.0]
   (if-let [[exponent & next-exponents] exponents]
     ;; sample MH transition at current exponent
     (let [;; Choose uniformly a random choice to resample.
           entry (rand-nth (get-trace state))
           ;; Compute next state from the resampled choice.
           proposal-state (next-state state entry)
           ;; Reconstruct the current state through transition back
           ;; from the next state; the rdb will be different.
           reverse-state (prev-state state proposal-state entry)
           ;; Apply Metropolis-Hastings acceptance rule to select
           ;; either the new or the current state.
           state (if (> (- (utility proposal-state exponent) 
                           (utility reverse-state exponent))
                        (Math/log (rand)))
                   proposal-state
                   state)
           ;; Update log weight for all but final step
           log-weight (if-let [next-exponent (first next-exponents)]
                        (+ log-weight
                           (* (get-log-weight state) 
                              (- next-exponent exponent)))
                        log-weight)]
       (recur next-exponents
              state
              log-weight))
     ;; we're done, return weighted sample
     (set-log-weight state log-weight))))

(defmethod infer :ais 
  [_ prog value 
   & {:keys [;; Number of annealing steps to perform per sample
             number-of-steps
             ;; Initial exponent for annealing schedule
             start-exponent
             ;; Final exponent for annealing schedule
             end-exponent]
      :or {number-of-steps 1
           start-exponent 0.0
           end-exponent 1.0}}]
   (letfn
    [(sample-seq []
       (lazy-seq
         (let [exponents (linspace 
                           start-exponent
                           end-exponent
                           (inc number-of-steps))
               state (anneal prog value exponents)]
           (cons state (sample-seq)))))]
     (sample-seq)))
