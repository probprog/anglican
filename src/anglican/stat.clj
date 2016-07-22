(ns anglican.stat
  "Statistics related functions"
  (:require [clojure.core.matrix :as m])
  (:use [anglican
           runtime
           [state :only [get-predicts get-log-weight]]]))

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
