(ns anglican.results-test
  (:require [clojure.test :refer [deftest testing is]]
            [anglican.results :refer [KL L2]]
            [anglican.runtime :refer [sqrt]]))

(deftest KL-test
  (testing "KL"
    (is (= 0. (KL {:x 0.9 :y 0.1} {:x 0.9 :y 0.1}))
        "same frequencies")
    (is (< 0. (KL {:x 0.25 :y 0.75} {:x 0.75 :y 0.25}) 1.)
        "different frequencies")
    (is (> 0. (KL {:x 0.25 :y 0.75} {:x 1.}))
        "missing p frequency")
    (is (< 0. (KL {:x 1.} {:x 0.5 :y 0.5}))
        "missing q frequency")))

        
(deftest L2-test
  (testing "L2"
    (is (= (L2 {:x 0.9 :y 0.1} {:x 0.9 :y 0.1}) 0.0)
        "same frequencies")
    (is (= (L2 {:x 0.25 :y 0.75} {:x 0.75 :y 0.25})
           (sqrt 0.5))
        "different frequencies")
    (is (= (L2 {:x 0.75 :y 0.25} {:x 1.}) 
          (sqrt 0.125))
        "missing p frequency")
    (is (= (L2 {:x 1.} {:x 0.5 :y 0.5})
           (sqrt 0.5))
        "missing q frequency")))

        
