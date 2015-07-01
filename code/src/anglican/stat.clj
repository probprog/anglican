(ns anglican.stat
  "Statistics related functions"
  (:require [clojure.core.matrix :as m])
  (:use [anglican
           runtime
           [state :only [get-predicts get-log-weight]]]))

(defn- square [x]
  "returns square of argument x, which may be an array"
  (m/mul x x))

(defn weighted-frequencies
  "applies f to each sample and returns a map {v W} containing
  the total weight W associated with each unique return value v"
  [f samples]
  (let [log-Z (- (reduce
                   log-sum-exp
                   (map get-log-weight samples))
                 (log (count samples)))]
    (reduce (fn [freqs s]
              (let [v (f s)]
                (assoc freqs
                  v (+ (get freqs v 0.0)
                       (exp (- (get-log-weight s)
                               log-Z))))))
            {}
            samples)))

(defn weighted-expectation
  "applies f to each sample and computes a weighted expectation value.
  f may return either a scalar or an array"
  [f samples]
  (let [vs (map f samples)
        lws (map get-log-weight samples)
        vlws (map vector vs lws)
        max-lw (reduce max lws)]
    (loop [vlws vlws
           sum-wv 0.0
           sum-w 0.0]
      (if-let [[v lw] (first vlws)]
        (let [w (exp (- lw max-lw))]
          (recur (rest vlws)
                 (m/add sum-wv (m/mul w v))
                 (m/add sum-w w)))
        (m/div sum-wv sum-w)))))

(defn l2-norm
  "calculates L2 norm (sum of squared differences) between a and b
  along specified dimension"
  ([a b dimension]
   (reduce
     m/add
     (map (fn [a-slice b-slice]
            (square (m/sub a-slice b-slice)))
          (m/slices a dimension)
          (m/slices b dimension))))
  ([a b]
   (l2-norm a b 0)))
