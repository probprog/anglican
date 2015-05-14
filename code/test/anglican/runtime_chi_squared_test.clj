(ns anglican.runtime-chi-squared-test
  "Tests for the Chi-squared distribution
  which implementation is in anglican/runtime"
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.core.matrix.stats :as matrix-stats])
  (:use anglican.runtime))

(defn sample-from-chi-squared
  "produces samples from the Chi-squared distribution"
  [nu number-of-samples]
  (let
    [distribution-instance (chi-squared nu)]
    (repeatedly
     number-of-samples
     (fn [] (sample distribution-instance)))))

(defn get-empirical-mean-for-chi-squared
  "returns an empirical mean for requested number
  of samples from the Chi-squared distribution"
  [nu number-of-samples]
  (matrix-stats/mean (sample-from-chi-squared nu number-of-samples)))

(defn get-empirical-variance-for-chi-squared
  "returns a matrix of variances for requested number
  of samples from the Chi-squared distribution"
  [nu number-of-samples]
  (matrix-stats/variance (sample-from-chi-squared nu number-of-samples)))

(defn get-theoretical-mean-for-chi-squared
  "returns an theoretical mean for the Chi-squared distribution"
  [nu]
  nu)

(defn get-theoretical-variance-for-chi-squared
  "returns a theoretical value of a matrix
  of variances for the Chi-squared distribution"
  [nu]
  (* 2 nu))

(defn chi-squared-mean-test-helper
  "a helper function to calculate the maximum absolute error
  between theoretical and empirical means"
  [nu number-of-samples]
  (Math/abs
    (-
     (get-empirical-mean-for-chi-squared nu number-of-samples)
     (get-theoretical-mean-for-chi-squared nu))))

(defn chi-squared-variance-test-helper
  "a helper function to calculate the maximum absolute error
  between theoretical and empirical variances"
  [nu number-of-samples]
  (Math/abs
    (-
     (get-empirical-variance-for-chi-squared nu number-of-samples)
     (get-theoretical-variance-for-chi-squared nu))))

(defn chi-squared-mean-test-assertion
  "a boolean assertion inequality
  for the mean of the Wishart distribution"
  [nu number-of-samples threshold]
  (< (chi-squared-mean-test-helper nu number-of-samples) threshold))

(defn chi-squared-variance-test-assertion
  "a boolean assertion inequality
  for the variance of the Wishart distribution"
  [nu number-of-samples threshold]
  (< (chi-squared-variance-test-helper nu number-of-samples) threshold))

(defn chi-squared-lnpdf-test-assertion
  "a boolean assertion inequality
  for the lnpdf of the Wishart distribution"
  [nu x value threshold]
  (< (abs (- (observe (chi-squared nu) x) value)) threshold))

(deftest test-chi-squared-observe
  (testing "Chi-squared lnpdf (observe). Deterministic test."
    ;; scipy.stats.chi2.pdf(1.0, 3.5) = 0.1962028031081274
    (is (chi-squared-lnpdf-test-assertion 3.5 1.0 (Math/log 0.1962028031081274) 0.001))

    ;; scipy.stats.chi2.pdf(5.31, 3.5) = 0.079546575897983585
    (is (chi-squared-lnpdf-test-assertion 3.5 5.31 (Math/log 0.079546575897983585) 0.001))

    ;; scipy.stats.chi2.pdf(5.31, 5.5) = 0.12068351943379794
    (is (chi-squared-lnpdf-test-assertion 5.5 5.31 (Math/log 0.12068351943379794) 0.001))))

(deftest test-chi-squared-sample
  ;; Nota bene. These tests are very approximate.
  ;;            They are not statistical tests.
  ;;            Threshold are selected by hand and very rough.
  (testing
    "Chi-squared sample. Stochastic test,
    passes with probability 0 < p < 1."

    (let
      [number-of-samples 100000]
      (let
        [nu 10.0]
        (is (chi-squared-mean-test-assertion
             nu number-of-samples 0.05))
        (is (chi-squared-variance-test-assertion
             nu number-of-samples 0.5)))

      (let
        [number-of-samples 100000]
        (let
          [nu 5.5]
          (is (chi-squared-mean-test-assertion
               nu number-of-samples 0.05))
          (is (chi-squared-variance-test-assertion
               nu number-of-samples 0.5))))

      (let
        [number-of-samples 100000]
        (let
          [nu 7.31]
          (is (chi-squared-mean-test-assertion
               nu number-of-samples 0.05))
          (is (chi-squared-variance-test-assertion
               nu number-of-samples 0.5))))

      (let
        [number-of-samples 100000]
        (let
          [nu 3.5]
          (is (chi-squared-mean-test-assertion
               nu number-of-samples 0.05))
          (is (chi-squared-variance-test-assertion
               nu number-of-samples 0.5)))))))
