(ns anglican.stat-test
  (:require [clojure.core.matrix :as m]
            [clojure.test :refer :all]
            [anglican.stat :refer :all]
            [anglican.runtime :refer :all]
            [anglican.state :refer [get-predicts get-log-weight]]))

(def tolerance 1e-12)

(def test-array
  (m/reshape (range 12) [3 4]))

(def test-weighted
  (for [x (range 11)]
    [x (observe (normal 5 2) x)]))

(deftest test-sum
  (is (m/equals [12 15 18 21]
                (sum test-array 0)))
  (is (m/equals [6 22 38]
                (sum test-array 1)))
  (is (m/equals (sum test-array)
                (sum test-array 0))))

(deftest test-mean
  (is (m/equals [12/3 15/3 18/3 21/3]
                (mean test-array 0)))
  (is (m/equals [6/4 22/4 38/4]
                (mean test-array 1)))
  (is (m/equals (mean test-array)
                (mean test-array 0))))

(deftest test-variance
  (is (m/equals [32/3 32/3 32/3 32/3]
                (variance test-array 0)))
  (is (m/equals [5/4 5/4 5/4]
                (variance test-array 1)))
  (is (m/equals (variance test-array)
                (variance test-array 0))))

(deftest test-covariance
  (is (m/equals [[32/3 32/3 32/3 32/3]
                 [32/3 32/3 32/3 32/3]
                 [32/3 32/3 32/3 32/3]
                 [32/3 32/3 32/3 32/3]]
                (covariance test-array 0)))
  (is (m/equals [[5/4 5/4 5/4]
                 [5/4 5/4 5/4]
                 [5/4 5/4 5/4]]
                (covariance test-array 1)))
  (is (m/equals (covariance test-array)
                (covariance test-array 0))))

(deftest test-std
  (is (m/equals (m/sqrt [32/3 32/3 32/3 32/3])
                (std test-array 0)))
  (is (m/equals (m/sqrt [5/4 5/4 5/4])
                (std test-array 1)))
  (is (m/equals (std test-array)
                (std test-array 0))))

(deftest test-l2
  (is (m/equals 48
                (l2-norm test-array (m/add test-array 2)))))

(deftest test-empirical-expectation
  (is (m/equals 5.0
                (empirical-expectation identity test-weighted)
                tolerance)))

(deftest test-empirical-mean
  (is (m/equals 5.0
                (empirical-mean test-weighted)
                tolerance)))

(deftest test-empirical-distribution
  (is (m/equals 1.0
                (reduce + (vals (empirical-distribution test-weighted)))
                tolerance)))
