(ns embang.trap-test
    (:require [clojure.test :refer [deftest testing is]])
    (use embang.trap))

(deftest test-simple?
  (testing "simple?"
    (is (simple? 1) "number")
    (is (simple? '(quote (1 2 3))) "quote")
    (is (simple? 'x) "variable")
    (is (simple? '(if 1 2 3)) "if with simple subexpressions")
    (is (not (simple? '(a b c))) "application")
    (is (not (simple? '(cond a 1 (b) 2)))
        "cond with compound subexpression")
    (is (simple? '(case x (foo bar)  3 4 5))
        "simple case without else")
    (is (simple? '(case x (foo bar) 3 5))
        "simple case with else")
    (is (not (simple? '(case x 3 (foo bar) 4 5)))
        "compound case without else")
    (is (not (simple? '(case x 1 3 (foo bar))))
        "compound case with else")
    (is (not (simple? '(fn [] 1))) "fn is never simple")))

(deftest test-primitive-procedure
  (binding [*gensym* symbol]
    (testing "primitive-procedure?"
      (is (primitive-procedure? 'inc) "inc is primitive")
      (is (not (primitive-procedure? 'fact))
          "fact is not primitive")
      (is (= (cps-of-expression '(fn [dec] dec) 'ret)
             '(ret (fn [C $state dec] (C dec $state)) $state))
          "primitive procedure name can be used as parameter")
      (is (= (cps-of-expression '(let [dec 1] dec) 'ret)
             '(let [dec 1] (ret dec $state)))
          "primitive procedure name can be locally rebound")
      (is (= (cps-of-expression '(let [x dec] (x 1)) 'ret)
             '(let [x (fn [C $state & P]
                        (C (apply dec P) $state))]
                (fn [] (x ret $state 1))))
          "primitive procedure can be locally bound")
      (is (= (cps-of-expression '(list inc dec) 'ret)
             '(ret (list 
                      (fn [C $state & P] (C (apply inc P) $state))
                      (fn [C $state & P] (C (apply dec P) $state)))
                   $state))
          "primitive procedure can be passed as an argument"))))

(deftest test-fn-cps
  (binding [*gensym* symbol]
    (testing "fn-cps"
      (is (= (fn-cps '([x] x))
             '(fn [C $state x] (C x $state)))
          "anonymous function")
      (is (= (fn-cps '(foo [x] (bar)))
             '(fn foo [C $state x] (fn [] (bar C $state))))
          "named function with compound body"))))

(deftest test-cps-of-let
  (binding [*gensym* symbol]
    (testing "cps-of-let"
      (is (= (cps-of-let '([x 1 y 2] (+ x y)) 'ret)
             '(let [x 1] (let [y 2] (ret (+ x y) $state))))
          "simple let")
      (is (= (cps-of-let '([x (foo 1)] x) 'ret)
             '(fn [] 
                (foo (fn [x $state] (ret x $state))
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
          "missing else"))
    (testing "cps-of-cond"
      (is (= (cps-of-cond '(1 2 3 4) 'ret)
             '(if 1 (ret 2 $state)
                (ret (cond 3 4) $state)))
          "cond via if"))

    (testing "cps-of-case"
      (is (= (cps-of-case '(x 1 (foo 2)) 'ret)
             '(case x 1 (fn [] (foo ret $state 2))))
          "opaque key")
      (is (= (cps-of-case '((foo x) 1 2) 'ret)
             '(fn []
                (foo (fn [K $state]
                       (ret (case K 1 2) $state))
                     $state x)))
          "translucent key"))

    (testing "cps-of-and"
      (is (= (cps-of-and nil 'ret)
             '(ret true $state))
          "empty")
      (is (= (cps-of-and '((y)) 'ret)
             '(fn [] (y ret $state)))
          "single argument")
      (is (= (cps-of-and '(x (y)) 'ret)
             '((fn [I $state]
                 (if I (fn [] (y ret $state))
                   (ret I $state)))
               x $state))
          "multiple arguments"))

    (testing "cps-of-or"
      (is (= (cps-of-or nil 'ret)
             '(ret false $state))
          "empty")
      (is (= (cps-of-or '((y)) 'ret)
             '(fn [] (y ret $state)))
          "single argument")
      (is (= (cps-of-or '(x (y)) 'ret)
             '((fn [I $state]
                (if I (ret I $state)
                  (fn [] (y ret $state))))
               x $state))
          "multiple arguments"))))

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
             '(ret (let [M (gensym "M")]
                     (fn [C $state & P]
                       (if (embang.state/in-mem? $state M P)
                         (C (embang.state/get-mem $state M P) $state)
                         (fn []
                           (clojure.core/apply
                             (fn [C $state x] (C x $state))
                             (fn [V $state]
                               (C V (embang.state/set-mem $state M P V)))
                             $state
                             P)))))
                   $state))
          "mem of compound function")
      (is (= (cps-of-mem '((fn foo [x] x)) 'ret)
             '(ret (let [M (gensym "M")]
                     (fn foo [C $state & P]
                       (if (embang.state/in-mem? $state M P)
                         (C (embang.state/get-mem $state M P) $state)
                         (fn []
                           (clojure.core/apply
                             (fn [C $state x] (C x $state))
                             (fn [V $state]
                               (C V (embang.state/set-mem $state M P V)))
                             $state
                             P)))))
                   $state))
          "mem of named compound function")
      (is (= (cps-of-mem '(foo) 'ret)
             '(ret (let [M (gensym "M")]
                     (fn [C $state & P]
                       (if (embang.state/in-mem? $state M P)
                         (C (embang.state/get-mem $state M P) $state)
                         (fn []
                           (clojure.core/apply
                             foo
                             (fn [V $state]
                               (C V (embang.state/set-mem $state M P V)))
                             $state
                             P)))))
                   $state))
          "mem of variable"))))

(deftest test-cps-of-store
  (binding [*gensym* symbol]
    (testing "cps-of-retrieve"
      (is (= (cps-of-retrieve [:a] 'ret)
             '(ret (embang.state/retrieve $state :a) $state))
          "retrieve"))
    (testing "cps-of-store"
      (is (= (cps-of-store [1] 'ret)
             '(ret 1 (embang.state/store $state 1)))
          "simple store")
      (is (= (cps-of-store [:a '(foo)] 'ret)
             '(fn [] (foo (fn [A $state] (ret A (embang.state/store $state :a A))) $state)))
          "compound store"))))

(deftest test-primitive-procedure-cps
  (binding [*gensym* symbol]
    (testing "primitive-procedure-cps"
      (is (= (primitive-procedure-cps 'inc)
             '(fn [C $state & P]
                (C (apply inc P) $state)))
          "primitive procedure"))))

(deftest test-cps-of-expression
  (binding [*gensym* symbol]
    (testing "(cps-of-expression simple-expression)"
      (is (= (cps-of-expression 1 'ret)
             '(ret 1 $state))
          "cps of integer")
      (is (= (cps-of-expression '(if 1 2 3) 'ret)
             '(ret (if 1 2 3) $state))
          "cps of simple if"))
    (testing "(cps-of-expression compound-expression)"
      (is (= (cps-of-expression '(if x (foo) (bar)) 'ret)
             '(if x (fn [] (foo ret $state)) (fn [] (bar ret $state))))
          "cps of compound if"))))

