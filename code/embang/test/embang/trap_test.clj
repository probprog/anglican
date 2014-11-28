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

(deftest test-primitive-procedure
  (binding [*gensym* symbol]
    (testing "primitive-procedure?"
      (is (primitive-procedure? 'inc) "inc is primitive")
      (is (not (primitive-procedure? 'fact))
          "fact is not primitive")
      (is (= (cps-of-expr '(fn [dec] dec) 'ret)
             '(ret (fn [C $state dec] (C dec $state)) $state))
          "primitive procedure name can be used as parameter")
      (is (= (cps-of-expr '(let [dec 1] dec) 'ret)
             '(let [dec 1] (ret dec $state)))
          "primitive procedure name can be locally rebound")
      (is (= (cps-of-expr '(let [x dec] (x 1)) 'ret)
             '((fn [V $state] (let [x V] (fn [] (x ret $state 1))))
               (fn
                 [C $state & P]
                 (C (apply dec P) $state))
               $state))
          "primitive procedure can be locally bound")
      (is (= (cps-of-expr '(list inc dec) 'ret)
             '((fn
                 [A $state]
                 ((fn [A $state] (ret (list A A) $state))
                  (fn [C $state & P] (C (apply dec P) $state))
                  $state))
               (fn [C $state & P] (C (apply inc P) $state))
               $state))
          "primitive procedure can be passed as an argument"))))

(deftest test-cps-of-fn
  (binding [*gensym* symbol]
    (testing "cps-of-fn"
      (is (= (cps-of-fn '([x] x) 'ret)
             '(ret (fn [C $state x] (C x $state)) $state))
          "anonymous function")
      (is (= (cps-of-fn '(foo [x] (bar)) 'ret)
             '(ret (fn foo [C $state x] (fn [] (bar C $state)))
                   $state))
          "named function with compound body"))))

(deftest test-cps-of-let
  (binding [*gensym* symbol]
    (testing "cps-of-let"
      (is (= (cps-of-let '([x 1 y 2] (+ x y)) 'ret)
             '(let [x 1] (let [y 2] (ret (+ x y) $state))))
          "simple let")
      (is (= (cps-of-let '([x (foo 1)] x) 'ret)
             '(fn [] 
                (foo (fn [V $state] (let [x V] (ret x $state)))
                     $state 1)))
          "compound value in let"))))

(deftest test-cps-of-if
  (binding [*gensym* symbol]
    (testing "defn-with-named-cont"
      (is (= (cps-of-if '(1 2 3) '(fn [c s] nil))
             '(let [C (fn [c s] nil)]
                (if 1 (C 2 $state) (C 3 $state))))
          "if with named cont"))
    (testing "cps-of-if"
      (is (= (cps-of-if '(1 2 3) 'ret)
             '(if 1 (ret 2 $state) (ret 3 $state)))
          "simple if")
      (is (= (cps-of-if '((a) (b) (c)) 'ret)
             '(fn []
                (a (fn [I $state] (if I
                                     (fn [] (b ret $state))
                                     (fn [] (c ret $state))))
                   $state)))
          "compound if")
      (is (= (cps-of-if '(1 2) 'ret)
             '(if 1 (ret 2 $state) (ret nil $state)))
          "missing else")
      (testing "cps-of-cond"
        (is (= (cps-of-cond '(1 2 3 4) 'ret)
               '(if 1 (ret 2 $state)
                    (if 3 (ret 4 $state)
                        (ret nil $state))))
            "cond via if")))))

(deftest test-cps-of-do
  (testing "cps-of-do"
    (is (= (cps-of-do '(1 2) 'ret)
           '((fn [_ $state] (ret 2 $state)) 1 $state))
        "list of simple expressions")
    (is (= (cps-of-do '((a) 1) 'ret)
           '(fn [] (a (fn [_ $state] (ret 1 $state)) $state)))
        "list of compound and simple")))

(deftest test-cps-of-apply
  (testing "cps-of-apply"
    (is (= (cps-of-apply '(+ terms) 'ret)
           '(ret (clojure.core/apply + terms) $state))
        "simple apply")
    (is (= (cps-of-apply '(foo xs) 'ret)
           '(fn [] (clojure.core/apply foo ret $state xs)))
        "compound apply")))

(deftest test-cps-of-predict
  (binding [*gensym* symbol]
    (testing "cps-of-predict"
      (is (= (cps-of-predict '('x x) 'ret)
             '(ret nil (embang.state/add-predict $state 'x x)))
          "simple predict")
      (is (= (cps-of-predict '('(foo) (foo)) 'ret)
             '(fn []
                (foo (fn [A $state]
                       (ret nil (embang.state/add-predict $state '(foo) A)))
                     $state)))))))

(deftest test-cps-of-sample
  (binding [*gensym* symbol]
    (testing "cps-of-sample"
      (is (= (cps-of-sample '((foo 2)) 'ret)
             '(fn [] (foo (fn [A $state]
                            (embang.trap/->sample 'S A ret $state))
                          $state
                          2)))
          "compound sample"))))

(deftest test-cps-of-observe
  (binding [*gensym* symbol]
    (testing "cps-of-observe"
      (is (= (cps-of-observe '((normal 1 1) 2) 'ret)
             '(embang.trap/->observe 'O (normal 1 1) 2 ret $state))
          "simple observe"))))

(deftest test-cps-of-mem
  (binding [*gensym* symbol]
    (testing "cps-of-mem"
      (is (= (cps-of-mem '((fn [x] x)) 'ret)
             '(ret (fn [C $state & P]
                     (if (embang.state/in-mem? $state 'M P)
                       (C (embang.state/get-mem $state 'M P) $state)
                       ((fn [A $state]
                          (fn []
                            (clojure.core/apply
                             A
                             (fn [V $state]
                               (C V (embang.state/set-mem $state 'M P V)))
                             $state
                             P)))
                        (fn [C $state x] (C x $state))
                        $state)))
                   $state))
          "mem of compound function"))))
