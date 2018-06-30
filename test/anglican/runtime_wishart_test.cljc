(ns anglican.runtime-wishart-test
  "Tests for the Wishart distribution
  which implementation is in anglican/runtime"
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.core.matrix.stats :as matrix-stats]
            [clojure.core.matrix :as m]
            [clojure.core.matrix
              :refer [identity-matrix mmul add sub transpose]
              :rename {identity-matrix eye
                       add madd
                       sub msub
                       transpose mtranspose}]
            [clojure.core.matrix.linear :as ml])
  (:use [anglican.runtime :only [abs pow observe* sample* 
                                 wishart gen-matrix]]))

(defn sample-from-wishart
  "produces samples from the Wishart distribution"
  [n V number-of-samples]
  (let
    [distribution-instance (wishart n V)]
    (repeatedly
     number-of-samples
     (fn [] (sample* distribution-instance)))))


;; begin work-around
;; cljs core.matrix backend on immutable persistent vectors
;; the matrix-stats namespace uses mutable matrices as accumulators

(defn mat-sum
  "Calculates the sum of a collection of values.
   Values may be:
    - A vector, in which case the result is a single scalar
    - A higher-dimensional array, in which case the result is the sum of all slices."
  ([values]
   (if (== 1 (m/dimensionality values))
     (m/esum values)
     (let [values (m/slices values)
           result (m/mutable (first values))]
       (reduce #(m/add %1 %2)
               result
               (next values))))))

(defn mat-mean
  "Calculates the mean of a collection of values.
   Values may be:
   - A vector, in which case the result is a single scalar
   - A higher-dimensional array, in which case the result is the mean of all slices."
  ([values]
   (let [values (m/slices values)
         n (m/dimension-count values 0)
         s (mat-sum values)]
     (if (number? s)
       (/ s n)
       (m/scale s (/ 1.0 n))))))

(defn mat-sum-of-squares
  "Calculates the sum of squares of a collection of values.
   Values may be:
   - A vector, in which case the result is a single scalar
   - A higher-dimensional array, in which case the result is the sum of squares in all slices."
  ([values]
   (if (== 1 (m/dimensionality values))
     (m/inner-product values values)
     (let [values (m/slices values)
           fv (first values)
           result (m/mutable (m/mul fv fv))]
       (reduce #(m/add %1 (m/mul %2 %2))
               result
               (next values))))))

(defn mat-variance
  "Calculates the unbiased sample variance of a set of values.
  Values may be scalars, vectors or higher-dimensional matrices."
  ([values]
   (let [n (m/dimension-count values 0)
         u (mat-mean values)
         ss (mat-sum-of-squares values)
         nuu (m/mul n (m/mul u u))]
     (if (number? ss)
       (* (- ss nuu) (/ 1.0 (dec n)))
       (do 
         (m/sub ss nuu)
         (m/scale ss (/ 1.0 (dec n)))
         ss)))))

;; end work-around

(defn get-empirical-mean-for-wishart
  "returns an empirical mean for requested number
  of samples from the Wishart distribution"
  [n V number-of-samples]
  (#?(:clj matrix-stats/mean
     :cljs mat-mean) (sample-from-wishart n V number-of-samples)))

(defn get-empirical-variance-for-wishart
  "returns a matrix of variances for requested number
  of samples from the Wishart distribution"
  [n V number-of-samples]
  (#?(:clj matrix-stats/variance
     :cljs mat-variance) (sample-from-wishart n V number-of-samples)))

(defn get-theoretical-mean-for-wishart
  "returns an theoretical mean for the Wishart distribution"
  [n V]
  (m/mul n V))

(defn get-theoretical-variance-for-wishart
  "returns a theoretical value of a matrix
  of variances for the Wishart distribution"
  [n V]
  (let
    [p (first (m/shape V))]
    (gen-matrix
     (fn [row column]
       (* n
          (+ (pow (m/mget V row column) 2.0)
             (* (m/mget V row row)
                (m/mget V column column)))))
     p p)))

(defn wishart-mean-test-helper
  "a helper function to calculate the maximum absolute error
  between theoretical and empirical means"
  [n V number-of-samples]
  (m/emax
   (m/abs
    (m/sub
     (get-empirical-mean-for-wishart n V number-of-samples)
     (get-theoretical-mean-for-wishart n V)))))

(defn wishart-variance-test-helper
  "a helper function to calculate the maximum absolute error
  between theoretical and empirical variances"
  [n V number-of-samples]
  (m/emax
   (m/abs
    (m/sub
     (get-empirical-variance-for-wishart n V number-of-samples)
     (get-theoretical-variance-for-wishart n V)))))

(defn wishart-mean-test-assertion
  "a boolean assertion inequality
  for the mean of the Wishart distribution"
  [n V number-of-samples threshold]
  (< (wishart-mean-test-helper n V number-of-samples) threshold))

(defn wishart-variance-test-assertion
  "a boolean assertion inequality
  for the variance of the Wishart distribution"
  [n V number-of-samples threshold]
  (< (wishart-variance-test-helper n V number-of-samples) threshold))

(defn wishart-lnpdf-test-assertion
  "a boolean assertion inequality
  for the lnpdf of the Wishart distribution"
  [n V x value threshold]
  (< (abs (- (observe* (wishart n V) x) value)) threshold))

(deftest test-wishart-observe
  ;; a function wishpdfln and an utility function logmvgamma
  ;; are provided in /resources/matlab/wishart/
  (testing "Wishart lnpdf (observe*). Deterministic test."
    ;; wishpdfln([1 0 ; 0 1], 3.5, [ 0.7 0 ; 0 0.07 ]) = -5.3950
    (is (wishart-lnpdf-test-assertion 3.5
                                      [[0.7 0] [0 0.07]]
                                      [[1.0 0.0] [0.0 1.0]]
                                      -5.3950
                                      0.001))

    ;; wishpdfln([3.1 -0.3; -0.3 11.0], 7.5, [ 3 2 ; 2 7 ]) = -12.5273
    (is (wishart-lnpdf-test-assertion 7.5
                                      [[3 2] [2 7]]
                                      [[3.1 -0.3]
                                       [-0.3 11.0]]
                                      -12.5273
                                      0.001))))

(let
    [n 10
     V [[0.7 0] [0 0.07]]
     number-of-samples 100]
  (wishart-mean-test-assertion n V number-of-samples 0.3))

(deftest test-wishart-sample
  ;; Nota bene. These tests are very approximate.
  ;;            They are not statistical tests.
  ;;            Threshold are selected by hand and very rough.
  (testing
    "Wishart sample. Stochastic test,
    passes with probability 0 < p < 1."

    (let
      [number-of-samples 10000]
      (let
        [n 10
         V [[0.7 0] [0 0.07]]]
        (is (wishart-mean-test-assertion
             n V number-of-samples 0.3))
        (is (wishart-variance-test-assertion
             n V number-of-samples 3.0)))

      (let
        [number-of-samples 100000]
        (let
          [n 5
           V [[0.7 0] [0 0.07]]]
          (is (wishart-mean-test-assertion
               n V number-of-samples 0.03))
          (is (wishart-variance-test-assertion
               n V number-of-samples 0.3))))

      (let
        [number-of-samples 100000]
        (let
          [n 5.5
           V [[0.7 0] [0 0.07]]]
          (is (wishart-mean-test-assertion
               n V number-of-samples 0.03))
          (is (wishart-variance-test-assertion
               n V number-of-samples 0.3))))

      (let
        [number-of-samples 100000]
        (let
          [n 3.5
           V [[3 2] [2 7]]]
          (is (wishart-mean-test-assertion
               n V number-of-samples 1.5))
          (is (wishart-variance-test-assertion
               n V number-of-samples 15.0)))))))
