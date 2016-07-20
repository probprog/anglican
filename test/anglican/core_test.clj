(ns anglican.core-test
  (:require [clojure.test :refer :all])
  (:use [anglican core emit runtime]))

(deftest stripdown-test
  (let [q (query []
            (let [theta (sample (normal 0 1))]
              (observe (gamma 1 1) theta)
              theta))]
    (is (every? (fn [s]
                  (> (:log-weight s) (/ -1.0 0.0)))
                (take 100 (doquery :importance q [])))
        (str "doquery should not return zero-weight samples when"
             ":stripdown option is set"))))