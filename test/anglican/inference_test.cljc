(ns anglican.inference-test
  "tests for anglican.inference"
  (:refer-clojure :exclude [rand rand-nth rand-int])
  (:require [clojure.test :refer [deftest testing is]])
  (:use [anglican.emit :only [query]]
        [anglican.inference :only [print-predict warmup]]))

(deftest test-print-predict 
    (testing "print-predict"
      (is (= (with-out-str
               (print-predict :anglican :a 1.0 0.0))
             #?(:clj ":a,1.0,0.0\n"
               :cljs ":a,1,0\n"))
          "anglican")
      (is (= (with-out-str
               (print-predict :json :a 1.0 0.0))
             #?(:clj "[\":a\",1.0,0.0]\n"
               :cljs "[\":a\",1,0]\n"))
          "json")
      (is (= (with-out-str
               (print-predict :clojure :a 1.0 0.0))
             #?(:clj "[:a 1.0 0.0]\n"
               :cljs "[:a 1 0]\n"))
          "clojure")
      (is (= (with-out-str
               (print-predict :default :a 1.0 0.0))
             (with-out-str
               (print-predict :anglican :a 1.0 0.0)))
          "default")))

(deftest test-warmup
  (testing "warmup"
    (is (some?  (warmup (query true)))
        "warmup with default value")
    (is (some?  (warmup (query true) nil))
        "warmup with explicit value")))
