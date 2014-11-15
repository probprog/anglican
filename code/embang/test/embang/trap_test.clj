(ns embang.trap-test
  (:require [clojure.test :refer [deftest testing is]])
  (:use embang.trap))

(deftest test-simplify
  (testing "simplify"
    (is (= (simplify '(let [x 1
                            y 2]
                        (if x y 0)))
                     '((fn [x y]
                         (cond x y :else 0))
                       1 2))
        "basic program")))

