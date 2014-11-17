(ns embang.trap-test
  (:require [clojure.test :refer [deftest testing is]])
  (use embang.trap))

(deftest test-simple-expr
  (testing "simple-expr?"
    (is (simple-expr? 1) "number")
    (is (simple-expr? '(quote (1 2 3))) "quote")
    (is (simple-expr? 'x) "variable")
    (is (simple-expr? '(if 1 2 3)) "if with simple subexpressions")
    (is (not (simple-expr? '(a b c))) "application")
    (is (not (simple-expr? '(cond a 1 (b) 2)))
        "cond with compound subexpression")
    (is (not (simple-expr? '(fn [] 1))) "fn is never simple")))

(deftest

    
