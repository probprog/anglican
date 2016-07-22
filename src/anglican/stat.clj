(ns anglican.stat
  "Statistics related functions"
  (:require [clojure.core.matrix :as m])
  (:use [anglican
           runtime
           [state :only [get-predicts get-log-weight]]]))

(defn- power [x n]
  (apply m/mul (repeat n x)))

(defn collect-by
  "calculates contribution to log marginal by value;
  - accepts a (finite) sequence of samples
  - returns a map containing the log sum weight weight for
    each unique value returned when applying f to a sample,
    normalized by the total number of samples"
  [f samples]
  (let [log-norm (Math/log (count samples))]
    (reduce (fn [weighted sample]
              (let [v (f sample)
                    lw (- (get-log-weight sample)
                          log-norm)]
                (if (weighted v)
                  (update-in weighted [v] log-sum-exp lw)
                  (assoc weighted v lw))))
            {}
            samples)))

(defn collect-predicts
  "constructs a map of weighted predict values 
  - accepts a key and a (finite) sequence of samples
  - returns a map {v log-w} containing an entry v for each unique
  predict value with key k, along with its log sum weight normalized
  by the total number of samples"
  [k samples]
  (collect-by #(get (get-predicts %) k)
              samples))

(defn collect-results
  "constructs a map of weighted result values 
  - accepts a (finite) sequence of samples
  - returns a map {v log-w} containing an entry v for each unique
  predict value along with its log sum weight normalized by the 
  total number of samples"
  [samples]
  (collect-by :result
              samples))

(defn empirical-distribution
  "calculates an empirical distribution from weighted samples;
  - accepts a map or sequence of log weighted values [x log-w]
  - returns a map {x p} of values and normalized probabilities"
  [weighted]
  (let [log-Z (reduce log-sum-exp (sort (map second weighted)))]
    (reduce (fn [dist [x log-w]]
              (assoc dist
                x (+ (dist x 0.0)
                     (exp (- log-w log-Z)))))
            {}
            weighted)))

(defn empirical-expectation
    "calculates an expected value from weighted samples;
  - accepts a function f and a map or sequence of log weighted
    values [x log-w]. f may return either a number or array.
  - returns the expected value of (f x)"
  [f weighted]
   (let [max-lw (reduce max (map second weighted))]
     (loop [weighted weighted
            sum-wv 0.0
            sum-w 0.0]
       (if-let [[x lw] (first weighted)]
         (let [v (f x)
               w (exp (- lw max-lw))]
           (recur (rest weighted)
                  (m/add (m/mul v w) sum-wv)
                  (+ sum-w w)))
         (m/div sum-wv sum-w)))))

(defn empirical-mean
  "calculates the mean from a collection of weighted pairs
  [x log-w], where x may be of a number or array"
  [weighted]
  (empirical-expectation identity weighted))

(defn empirical-covariance
  "calculates the covariance from a collection of weighted pairs
  [x log-w], where x may be of a number or vector."
  [weighted]
  (let [mu (empirical-mean weighted)
        dx (map #(m/sub % mu) 
                (map first weighted))]
    (empirical-expectation #(m/outer-product % %) 
                           (map vector dx (map second weighted)))))

(defn empirical-variance
  "calculates the variance from a collection of weighted pairs
  [x log-w], where x may be of a number or array"
  [weighted]
  (let [mu (empirical-mean weighted)]
    (empirical-expectation #(power (m/sub % mu) 2) weighted)))

(defn empirical-std
  "calculates the standard deviation from a collection of weighted
  values [x log-w], where x may be of a number or array"
  [weighted]
  (m/sqrt (empirical-variance weighted)))

(defn empirical-skew
  "calculates the standardized skew from a collection of weighted
  values [x log-w], where x may be of a number or array"
  [weighted]
  (let [mu (empirical-mean weighted)
        s (empirical-std weighted)]
    (empirical-expectation #(power (m/div (m/sub % mu) s) 3) weighted)))

(defn empirical-kurtosis
  "calculates the standardized kurtosis from a collection of weighted
  values [x log-w], where x may be of a number or array"
  [weighted]
  (let [mu (empirical-mean weighted)
        s (empirical-std weighted)]
    (empirical-expectation #(power (m/div (m/sub % mu) s) 4) weighted)))


(defn sum
  "sums array slices along specified dimension"
  ([a dimension]
   (reduce
     m/add
     (m/slices a dimension)))
  ([a]
   (sum a 0)))

(defn mean
  "mean of array slices along specified dimension"
  ([a dimension]
   (m/div (sum a dimension)
          (get (m/shape a) dimension)))
  ([a]
   (mean a 0)))

(defn variance
  "variance of array slices along specified dimension"
  ([a dimension]
   (let [e-a (mean a dimension)
         e-a2 (mean (power a 2) dimension)]
     (m/sub e-a2 (power e-a 2))))
  ([a]
   (variance a 0)))

(defn covariance
  "covariance of array slices along specified dimension"
  ([x dimension]
   (let [x-mean (mean x dimension)]
     (mean (map (fn [x-slice]
                  (let [dx-slice (m/sub x-slice x-mean)]
                    (m/outer-product dx-slice dx-slice)))
                (m/slices x dimension)))))
  ([x]
   (covariance x 0)))

(defn std
  "standard deviation (sqrt of variance) of array slices
  along specified dimension"
  ([a dimension]
   (m/sqrt (variance a dimension)))
  ([a]
   (std a 0)))

(defn skew
  "standardized skew of array slices along specified dimension"
  ([a dimension]
   (let [mu (mean a dimension)
         s (std a dimension)]
     (power (m/div (m/sub a mu) s) 3)))
  ([a]
   (skew a 0)))

(defn kurtosis
  "standardized skew of array slices along specified dimension"
  ([a dimension]
   (let [mu (mean a dimension)
         s (std a dimension)]
     (power (m/div (m/sub a mu) s) 4)))
  ([a]
   (kurtosis a 0)))

(defn l2-norm
  "calculates L2 norm (sum of squared differences) between a and b"
  [a b]
  (reduce +
          (map #(power (- %1 %2) 2)
               (m/to-vector a)
               (m/to-vector b))))


