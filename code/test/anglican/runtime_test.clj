(ns anglican.runtime-test
  (:require [clojure.test :refer [deftest testing is]])
  (:use anglican.runtime))

(deftest test-categorical
  (testing "categorical"
    (let [dist (categorical '((x 1) (y 2)))]
      (is (= (observe dist 'x) (Math/log (/ 1. 3.)))
          "observing value in support")
      (is (= (observe dist 'z) (Math/log 0.))
          "observing value not in support"))))

(deftest test-uniform-discrete
  (testing "uniform-discrete"
    (let [dist (uniform-discrete 0 3)]
      (is (= (observe dist 1) (Math/log (/ 1. 3.)))
          "values in domain are uniformly distributed")
      (is (= (observe dist 3) (Math/log 0.))
          "upper bound is not in the domain")
      (is (= (observe dist -1) (Math/log 0.))
          "values not in the range have zero probability")
      (is (= (observe dist 0.5) (Math/log 0.))
          "values of wrong type have zero probability"))))

(deftest test-CRP
  (testing  "CRP"
    (let [proc (CRP 1.0)]
      (is (= (observe (produce (absorb proc 3)) 3) (Math/log 1/2))
          "observing absorbed value")
      (is (= (observe (produce (absorb proc 0)) 1) (Math/log 1/2))
          "observing unabsorbed value greater than count")
      (is (= (observe (produce (absorb proc 1)) 0) (Math/log 1/2))
          "observing unabsorbed value less than count")
      (is (= (observe (produce proc) 2) (Math/log 1))
          "observing any new value"))))

(deftest test-cov
  (testing "cov"
    (is (= (cov + [1 2] [1 3]) [[2 4] [3 5]])
        "square matrix")
    (is (= (cov * [1 2] [3]) [[3] [6]])
        "vector to scalar")
    (is (= (cov str "c" ["a" "b"]) [["ca" "cb"]])
        "scalar to vector")))
