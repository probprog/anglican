(ns anglican.semantic-test
  (:require [clojure.test :refer [deftest testing is]]
            [anglican.state :refer [get-result]])
  (:use [anglican.core :only [doquery]]
        [anglican.emit :only [query]]
        [anglican.runtime :only []]))


(deftest catch-throw-test
  (let [q (query [x]
                 (let [a 5]
                   (+ a x)))
        result (get-result (first (doquery :importance q [4])))]
    (is (= result 9)
        "no catch, no throws"))
  (let [q (query [x]
                 (catch :c
                   (catch :b 
                     (let [a (catch :a (throw :a 5))]
                       (throw :b (+ a x))
                       10))))
        result (get-result (first (doquery :importance q [4])))]
    (is (= result 9)
        "three catches, two throws"))
  (let [q (query [x]
                 (let [a 5]
                   (throw :a x)
                   a))]
    (is (thrown? #?(:clj RuntimeException
                   :cljs js/Object)
                 (get-result (first (doquery :importance q [4]))))
        "no catch, one throw should throw a runtime exception")
    (is (try 
          (get-result (first (doquery :importance q [4])))
          (catch #?(:clj clojure.lang.ExceptionInfo
                   :cljs js/Object) e
            (= (ex-data e)
               {:error-type :anglican-uncaught-throw
                :throw-tag :a
                :value 4})))
        "clojure try-catch to catch an anglican-uncaught-throw"))
  (let [q (query [a]
                 (catch :max
                   (when (> a 0)
                     (throw :max a))
                   0))]
    (is (= (get-result (first (doquery :importance q [4]))) 4)
        "max(0, 4) is 4")
    (is (= (get-result (first (doquery :importance q [-1]))) 0)
        "max(0, -1) is -1"))
  (let [q (query [a b c]
                 (catch :foo
                   (do
                     (when (< a 0)
                       (throw :foo b))
                     a)
                   c))]
    (is (= (get-result (first (doquery :importance q [-1 2 3]))) 2)
        "when a is less than 0, return b")
    (is (= (get-result (first (doquery :importance q [1 2 3]))) 3)
        "when a is not loss than 0, return c")))
