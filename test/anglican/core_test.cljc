(ns anglican.core-test
  (:require [clojure.test :refer [deftest testing is]])
  (:use [anglican.core :only [doquery]]
        [anglican.emit :only [query]]
        [anglican.runtime :only [normal gamma]]))

(deftest stripdown-test
  (let [q (query []
                 (let [theta (sample (normal 0 1))]
                   (observe (gamma 1 1) theta)
                   theta))]
    (is (every? (fn [s]
                  (> (:log-weight s) (/ -1.0 0.0)))
                (take 100 (doquery :importance q [] 
                                   :drop-invalid true)))
        (str "doquery should not return zero-weight samples when"
             ":drop-invalid option is set to true"))
    (is (some (fn [s]
                (= (:log-weight s) (/ -1.0 0.0)))
              (take 100 (doquery :importance q [] 
                                 :drop-invalid false)))
        (str "doquery should return zero-weight samples when"
             ":drop-invalid option is set to false"))
    (is (every? (fn [s]
                  (> (:log-weight s) (/ -1.0 0.0)))
                (take 100 (doquery :importance q [])))
        (str "doquery should not return zero-weight samples when"
             ":drop-invalid option is not specified"))))
