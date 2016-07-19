(ns anglican.rmh-test
  (:require [clojure.test :refer [deftest testing is]])
  (:use anglican.rmh
        anglican.rmh-dists
        [anglican.runtime :only [observe sample]]))

(deftest test-rmh-dists
  (testing "folded-normal-dist"
    (let [dist (folded-normal 4 8)]
      (is (and (number? (sample dist))
               (>= (sample dist) 0)
               (= (/ -1 0.) (observe dist -0.00000000001))
               (not= (/ -1 0.) (observe dist 0)))
          "testing folded-normal-dist")))
  (testing "folded-normal-positive-dist"
    (let [dist (folded-normal-positive 0 1)]
      (is (and (number? (sample dist))
               (> (sample dist) 0)
               (= (/ -1 0.) (observe dist 0))
               (not= (/ -1 0.) (observe dist 0.0000001)))
          "testing folded-normal-positive-dist")))
  (testing "folded-normal-discrete-dist"
    (let [dist (folded-normal-discrete 4 8)]
      (is (and (integer? (sample dist))
               (= (/ -1 0.) (observe dist 0.4))
               (= (/ -1 0.) (observe dist -1))
               (not= (/ -1 0.) (observe dist 0))
               (not= (/ -1 0.) (observe dist 4)))
          "testing folded-normal-discrete-dist"))))

(deftest test-alt-proposals
  (testing "alt-proposal for exponential"
    (let [dist (anglican.runtime/exponential 1)
          alt-dist (anglican.rmh/get-alt-proposal dist 1 2)]
      (is (instance? anglican.rmh_dists.folded-normal-distribution alt-dist))))
  (testing "alt-proposal for gamma"
    (let [dist (anglican.runtime/gamma 1 2)
          alt-dist (anglican.rmh/get-alt-proposal dist 1 2)]
      (is (instance? anglican.rmh_dists.folded-normal-positive-distribution alt-dist))))
  (testing "alt-proposal for normal"
    (let [dist (anglican.runtime/normal 1 2)
          alt-dist (anglican.rmh/get-alt-proposal dist 1 2)]
      (is (instance? anglican.runtime.normal-distribution alt-dist))))
  (testing "alt-proposal for poisson"
    (let [dist (anglican.runtime/poisson 1)
          alt-dist (anglican.rmh/get-alt-proposal dist 1 2)]
      (is (instance? anglican.rmh_dists.folded-normal-discrete-distribution alt-dist))))
  (testing "alt-proposal for beta"
    (let [dist (anglican.runtime/beta 1 2)
          alt-dist (anglican.rmh/get-alt-proposal dist 1 2)]
      (is (instance? anglican.runtime.uniform-continuous-distribution alt-dist)))))
