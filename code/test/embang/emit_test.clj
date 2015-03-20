(ns embang.emit-test
  (:require [clojure.test :refer [deftest testing is]])
  (:use [embang.trap :only [value-cont]]
        embang.emit))

(deftest test-higher-order-functions
  (testing "map in CPS"
    (is (= (trampoline
             ($map value-cont nil (cps-fn [lst] (first lst))
                   '((1) (2) (3))))
           '(1 2 3))
        "map on a single list")
    (is (= (trampoline
             ($map value-cont nil (cps-fn [x y] (+ x y))
                   '( 1 2 3) '(4 5 6)))
           '(5 7 9))
        "map on two lists"))

  (testing "reduce in CPS"
    (is (= (trampoline
             ($reduce value-cont nil (cps-fn [x y] (+ x y))
                      '(1 2 3)))
           6)
        "reduce without default")
    (is (= (trampoline
             ($reduce value-cont nil (cps-fn [x y] (conj x y))
                      nil
                      '(1 2 3)))
           '(3 2 1))
        "reduce with default"))

  (testing "filter in CPS"
    (is (= (trampoline
             ($filter value-cont nil (cps-fn [x] (odd? x))
                      '(1 2 3)))
           '(1 3))
        "filter"))

  (testing "repeatedly in CPS"
    (is (= (trampoline
             ($repeatedly value-cont nil 3 (cps-fn [] 1)))
           '(1 1 1))))

  (testing "comp in CPS"
    (is (= (trampoline
             (($comp value-cont nil)) value-cont nil 1)
           1)
        "argumentless comp")
    (is (= (trampoline
             ((($comp value-cont nil
                      (cps-fn [x y] (+ x y))))
              value-cont nil 1 2))
           3)
        "single-argument comp")
    (is (= (trampoline
             ((($comp value-cont nil 
                      (cps-fn [x] (odd? x))
                      (cps-fn [& terms] (apply + terms))))
              value-cont nil 1 1 1))
           true)
        "multi-argument comp"))

  (testing "partial in CPS"
    (is (= (trampoline
             ((($partial value-cont nil
                         (cps-fn [x y] (+ x y))
                         1))
              value-cont nil 2))
           3)
        "partially applied +")))
