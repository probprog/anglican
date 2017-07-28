(ns anglican.catch-throw-test
  (:require [clojure.test :refer :all]
            [anglican.state :refer [get-result]])
  (:use [anglican core emit runtime]))

(deftest catch-throw-test
  (let [q (query [x]
                 (let [a 5]
                   (+ a x)))
        result (get-result (first (doquery :importance q [4])))]
    (is (= result 9)
        "no catch, no throws"))
  (let [q (query [x]
                 (let [a 5]
                   (throw :a x)
                   a))]
    (is (thrown? RuntimeException
                 (get-result (first (doquery :importance q [4]))))
        "no catch, one throw")
    (is (try 
          (get-result (first (doquery :importance q [4])))
          (catch clojure.lang.ExceptionInfo e
            (= (ex-data e)
               {:error-type :anglican-uncaught-throw
                :throw-tag :a
                :value 4})))
        "clojure try-catch to catch an anglican-uncaught-throw"))
  (let [q (query [x]
                 (catch :b 
                   (let [a (catch :a (throw :a 5))]
                     (throw :b (+ a x))
                     10)))
        result (get-result (first (doquery :importance q [4])))]
    (is (= result 9)
        "two catches, two throws"))
  (let [q (query [a]
                 (catch :max
                   (when (> a 0)
                     (throw :max a))
                   0))]
    (is (= (get-result (first (doquery :importance q [4]))) 4)
        "one catch, one throw")
    (is (= (get-result (first (doquery :importance q [-1]))) 0)
        "one catch, one throw"))
  (let [q (query [x]
                 (catch :d (catch :c (catch :b (catch :a x)))))
        result (get-result (first (doquery :importance q [4])))]
    (is (= result 4)
        "four catches, no throws")))
