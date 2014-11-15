(ns embang.xlat_test
  (:require [clojure.test :refer [deftest testing is]])
  (:use embang.xlat))

(deftest test-elist
  (testing "elist"
    (is (= (elist '(1 2)) '(1 2)) "simple list")
    (is (= (elist '((define x 1) x)) '((let [x 1] x))) "define in list")
    (is (= (elist '((define y 2))) '((let [y 2]))) "define is last")))

(deftest test-alambda
  (testing "alambda"
    (is (= (alambda nil '((a b c) a b c))
           '(fn [a b c] a b c))
        "simple")
    (is (= (alambda nil '(a a))
           '(fn [& a] a))
        "variadic")
    (is (= (alambda 'foo '((x) (* x x)))
           '(fn foo [x] (* x x)))
        "named")))

(deftest test-alet
  (testing "alet"
    (is (= (alet '(((x 1) (y 2)) (+ x y))) '(let [x 1 y 2] (+ x y))) "let")))

(deftest test-acond
  (testing "acond"
    (is (= (acond '(((= x 1) x) ((= x 2) (* 2 x))))
           '(cond (= x 1) x (= x 2) (* 2 x))) "without else")
    (is (= (acond '(((= x 1) x) (else (* 2 x))))
           '(cond (= x 1) x :else (* 2 x))) "with else")))

(deftest test-abegin
  (testing "abegin"
    (is (= (abegin '((do 1) 2)) '(do (begin 1) 2)) "begin with name clash")))

(deftest test-aform
  (testing "aform"
    (is (= (aform '(fn 1 2)) '(lambda 1 2)) "application with name clash")))

(deftest test-expression
  (testing "expression"
    (is (= (expression '()) ()) "empty list")))

(deftest test-dlist
  (testing "dlist"
    (is (= (dlist '([assume x 1]
                    [assume y 2]
                    [predict y]
                    [observe (normal x 1) 1]))
           '((let [x 1]
               (let [y 2]
                 (predict y)
                 (observe (normal x 1) 1)))))
        "assumes, predict, observe")))

(deftest test-anglican
  (testing "anglican"
    (is (= (anglican
            '[[assume x 1]
              [assume y 2]
              [predict (+ x y)]])
           '(fn []
              (let [x 1]
                (let [y 2]
                  (predict (+ x y))))))
        "simple program")
    (is (= (anglican
            '[[assume fact (lambda (n)
                              (if (= n 1)
                                1 
                                (* n (fact (- n 1)))))]
              [predict (fact 5)]])
           '(fn []
              (let [fact (fn fact [n]
                           (if (= n 1)
                             1 
                             (* n (fact (- n 1)))))]
                (predict (fact 5)))))
        "auto-recursive function")))
                            
                            
