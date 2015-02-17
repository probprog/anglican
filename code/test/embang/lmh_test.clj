(ns embang.lmh-test
  (:require [clojure.test :refer [deftest testing is]])
  (:use embang.lmh))

(deftest test-rdb
  (testing "rdb"
    (is (= (rdb [(->entry 1 1 nil nil) (->entry 2 10 nil nil)])
           {1 1, 2 10})
        "making rdb from trace")))
