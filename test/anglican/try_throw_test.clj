(ns anglican.try-throw-test
  (:require [clojure.test :refer :all]
            [anglican.state :refer [get-result]])
  (:use [anglican core emit runtime]))

(deftest try-throw-test
  (let [q (query [x]
                 (let [a 5]
                   (+ a x)))
        result (get-result (first (doquery :importance q [4])))]
    (is (= result 9)
        "implicit global try, no throws"))
  (let [q (query [x]
                 (let [a 5]
                   (throw x)
                   a))
        result (get-result (first (doquery :importance q [4])))]
    (is (= result 4)
        "implicit global try, one throw"))
  (let [q (query [x]
                 (let [a (try (throw 5))]
                   (throw (+ a x))
                   10))
        result (get-result (first (doquery :importance q [4])))]
    (is (= result 9)
        "two trys (one implicit global try), two throws"))
  (let [q (query [x]
                 (let [a (try (throw 5))]
                   (+ a x)))
        result (get-result (first (doquery :importance q [4])))]
    (is (= result 9)
        "two trys (one implicit global try), one throw"))
  (let [q (query [x]
                 (try (try (try (try x)))))
        result (get-result (first (doquery :importance q [4])))]
    (is (= result 4)
        "five trys (one implicit global try), no throws")))
