(ns anglican.stat-test
  (:require [clojure.core.matrix :as m]
            [clojure.test :refer :all]
            [anglican.stat :refer :all]
            [anglican.runtime :refer :all]
            [anglican.state :refer [get-predicts get-log-weight]]))

(def tolerance 1e-12)

(def test-array
  (m/reshape (range 12) [3 4]))

(def test-samples
  (map (fn [x]
         {:anglican.state/log-weight (observe (normal 5 2) x)
          :anglican.state/predicts [[:x x]]})
       (range 11)))

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

(deftest test-std
  (is (m/equals (m/sqrt [32/3 32/3 32/3 32/3])
                (std test-array 0)))
  (is (m/equals (m/sqrt [5/4 5/4 5/4])
                (std test-array 1)))
  (is (m/equals (std test-array)
                (std test-array 0))))


(deftest test-l2
  (is (m/equals [12 12 12 12]
                (l2-norm test-array (m/add test-array 2) 0)))
  (is (m/equals [16 16 16]
                (l2-norm test-array (m/add test-array 2) 1)))
  (is (m/equals (l2-norm test-array (m/add test-array 2))
                (l2-norm test-array (m/add test-array 2) 0))))

(deftest test-weighted-expectation
  (is (m/equals 5.0
                (weighted-expectation (comp :x get-predicts)
                                      test-samples)
                tolerance)))

(deftest test-weighted-frequencies
  (is (m/equals (count test-samples)
                (->> test-samples
                     (weighted-frequencies (comp #(mod % 2) :x get-predicts))
                     vals
                     (reduce +))
                tolerance)))


