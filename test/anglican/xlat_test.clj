(ns anglican.xlat-test
  (:require [clojure.test :refer [deftest testing is]])
  (:use anglican.xlat))

(deftest test-elist
  (testing "elist"
    (is (= (elist '(1 2)) '(1 2)) "simple list")
    (is (= (elist '((define x 1) x)) '((let [x 1] x)))
        "define in list")
    (is (= (elist '((define y 2))) '((let [y 2])))
        "define is last")))

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
    (is (= (alet '(((x 1) (y 2)) (+ x y)))
           '(let [x 1 y 2] (+ x y)))
        "let")))

(deftest test-aloop
  (testing "aloop"
    (is (= (aloop '(((x 1) (y 2)) (recur (+ x 1) y)))
           '((fn loop [x y] (recur (+ x 1) y)) 1 2))
        "loop")))

(deftest test-acond
  (testing "acond"
    (is (= (acond '(((= x 1) x) ((= x 2) (* 2 x))))
           '(cond (= x 1) x (= x 2) (* 2 x))) "without else")
    (is (= (acond '(((= x 1) x) (else (* 2 x))))
           '(cond (= x 1) x :else (* 2 x))) "with else")))

(deftest test-acase
  (testing "acase"
    (is (= (acase '(x (1 2) (3 4)))
           '(case x 1 2 3 4)) "without else")
    (is (= (acase '((+ y z) (1 2) (3 4) (else 5)))
           '(case (+ y z) 1 2 3 4 5)))))

(deftest test-abegin
  (testing "abegin"
    (is (= (abegin '((do 1) 2)) '(do (begin 1) 2))
        "begin with name clash")))

(deftest test-aform
  (testing "aform"
    (is (= (aform '(fn 1 2)) '(lambda 1 2))
        "application with name clash")))

(deftest test-expression
  (testing "expression"
    (is (= (expression '()) ()) "empty list")))

(deftest test-program
  (testing "program"
    (is (= (program
            '[[assume x 1]
              [assume y 2]
              [predict (+ x y)]])
           '(do
              (let [x 1]
                (let [y 2]
                  (predict '(+ x y) (+ x y))))))
        "simple program")
    (is (= (program
            '[[assume fact (lambda (n)
                              (if (= n 1)
                                1 
                                (* n (fact (- n 1)))))]
              [predict (fact 5)]])
           '(do
              (let [fact (fn fact [n]
                           (if (= n 1)
                             1 
                             (* n (fact (- n 1)))))]
                (predict '(fact 5) (fact 5)))))
        "auto-recursive function")))
                            
                            
