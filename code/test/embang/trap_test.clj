(ns embang.trap-test
    (:require [clojure.test :refer [deftest testing is]])
    (use embang.trap))

(deftest test-simple?
  (testing "simple?"
    (is (simple? 1) "number")
    (is (simple? [1 2]) "vector")
    (is (simple? {:x 1}) "map")
    (is (simple? #{1}) "set")
    (is (simple? '(quote (1 2 3))) "quote")
    (is (simple? 'x) "variable")
    (is (simple? '(if 1 2 3)) "if with simple subexpressions")
    (is (not (simple? '[1 (foo x)]))
        "vector with compound subexpressions")
    (is (not (simple? '{:x (bar 1)}))
        "map with compound subexpressions")
    (is (not (simple? '#{(if x (foo a) (bar b))}))
        "set with compound subexpressions")
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
             '(fn []
                (ret (fn fn [C $state dec] 
                       (fn [] (C dec $state))) $state)))
          "primitive procedure name can be used as parameter")
      (is (= (cps-of-expression '(let [dec 1] dec) 'ret)
             '(let [dec 1]
                (fn [] (ret dec $state))))
          "primitive procedure name can be locally rebound")
      (is (= (cps-of-expression '(let [x dec] (x 1)) 'ret)
             '(let [x (fn dec [C $state & P]
                        (fn [] (C (apply dec P) $state)))]
                (x ret $state 1)))
          "primitive procedure can be locally bound")
      (is (= (cps-of-expression '(list inc dec) 'ret)
             '(fn []
                (ret (list
                       (fn inc [C $state & P]
                         (fn [] (C (apply inc P) $state)))
                       (fn dec [C $state & P] 
                         (fn [] (C (apply dec P) $state))))
                     $state)))
          "primitive procedure can be passed as an argument"))))

(deftest test-of-literal
  (testing "vector"
    (is (= (cps-of-vector [1 2 3] 'ret)
           '(fn [] (ret (vector 1 2 3) $state)))
        "literal vector"))
  (testing "map"
    (is (= (cps-of-hash-map {1 2} 'ret)
           '(fn [] (ret (hash-map 1 2) $state)))
        "literal hash-map"))
  (testing "set"
    (is (= (cps-of-set #{1} 'ret)
           '(fn [] (ret (set (list 1)) $state)))
        "literal set")))

(deftest test-fn-cps
  (binding [*gensym* symbol]
    (testing "fn-cps"
      (is (= (fn-cps '([x] x))
             '(fn fn [C $state x] (fn [] (C x $state))))
          "anonymous function")
      (is (= (fn-cps '(foo [x] (bar)))
             '(fn foo [C $state x] (bar C $state)))
          "named function with compound body"))))

(deftest test-cps-of-let
  (binding [*gensym* symbol]
    (testing "cps-of-let"
      (is (= (cps-of-let '([x 1 y 2] (+ x y)) 'ret)
             '(let [x 1]
                (let [y 2]
                  (fn [] (ret (+ x y) $state)))))
          "simple let")
      (is (= (cps-of-let '([x (foo 1)] x) 'ret)
             '(foo (fn var [x $state]
                     (fn [] (ret x $state)))
                   $state 1))
          "compound value in let"))))

(deftest test-cps-of-if
  (binding [*gensym* symbol]
    (testing "defn-with-named-cont"
      (is (= (cps-of-if '(1 2 3) '(fn cont [c s] nil))
             '(let [C (fn cont [c s] nil)]
                (if 1
                  (fn [] (C 2 $state))
                  (fn [] (C 3 $state)))))
          "if with named cont"))

    (testing "cps-of-if"
      (is (= (cps-of-if '(1 2 3) 'ret)
             '(if 1
                (fn [] (ret 2 $state))
                (fn [] (ret 3 $state))))
          "simple if")
      (is (= (cps-of-if '((a) (b) (c)) 'ret)
             '(a (fn if [I $state]
                   (if I
                     (b ret $state)
                     (c ret $state)))
                 $state))
          "compound if")
      (is (= (cps-of-if '(1 2) 'ret)
             '(if 1 
                (fn [] (ret 2 $state))
                (fn [] (ret nil $state))))
          "missing else"))

    (testing "cps-of-when"
      (is (= (cps-of-when '(a b c) 'ret)
             (cps-of-expression '(if a (do b c)) 'ret))
          "when via if"))

    (testing "cps-of-cond"
      (is (= (cps-of-cond '(1 2 3 (foo 4)) 'ret)
             (cps-of-expression '(if 1 2 (if 3 (foo 4) nil)) 'ret))
          "cond via if"))

    (testing "cps-of-case"
      (is (= (cps-of-case '(x 1 (foo 2)) 'ret)
             '(case x 1 (foo ret $state 2)))
          "opaque key")
      (is (= (cps-of-case '((foo x) 1 2) 'ret)
             '(foo (fn case [K $state] 
                     (fn [] (ret (case K 1 2) $state)))
                   $state x))
          "translucent key"))

    (testing "cps-of-and"
      (is (= (cps-of-and nil 'ret)
             '(fn [] (ret true $state)))
          "empty")
      (is (= (cps-of-and '((y)) 'ret)
             '(y ret $state))
          "single argument")
      (is (= (cps-of-and '(x (y)) 'ret)
             '(fn []
                ((fn and [I $state]
                   (if I
                     (y ret $state)
                     (fn [] (ret I $state))))
                 x $state)))
          "multiple arguments"))

    (testing "cps-of-or"
      (is (= (cps-of-or nil 'ret)
             '(fn [] (ret false $state)))
          "empty")
      (is (= (cps-of-or '((y)) 'ret)
             '(y ret $state))
          "single argument")
      (is (= (cps-of-or '(x (y)) 'ret)
             '(fn []
                ((fn or [I $state]
                   (if I
                     (fn [] (ret I $state))
                     (y ret $state)))
                 x $state)))
             "multiple arguments"))))

(deftest test-cps-of-do
  (binding [*gensym* symbol]
    (testing "cps-of-do"
      (is (= (cps-of-do '(1 2) 'ret)
             '(fn [] ((fn elist [_ $state] 
                        (fn [] (ret 2 $state)))
                      1 $state)))
          "list of simple expressions")
      (is (= (cps-of-do '((a) 1) 'ret)
             '(a (fn elist [_ $state] (fn [] (ret 1 $state)))
                 $state))
          "list of compound and simple"))))

(deftest test-cps-of-apply
  (binding [*gensym* symbol]
    (testing "cps-of-apply"
      (is (= (cps-of-apply '(+ terms) 'ret)
             '(fn [] (ret (clojure.core/apply + terms) $state)))
          "simple apply")
      (is (= (cps-of-apply '(foo xs) 'ret)
             '(clojure.core/apply foo ret $state xs))
          "compound apply"))))

(deftest test-cps-of-predict
  (binding [*gensym* symbol]
    (testing "cps-of-predict"
      (is (= (cps-of-predict '('x x) 'ret)
             '(fn []
                (ret nil (embang.state/add-predict $state 'x x))))
          "simple predict")
      (is (= (cps-of-predict '('(foo) (foo)) 'ret)
             '(foo (fn arg [A $state]
                     (fn []
                       (ret nil (embang.state/add-predict $state
                                                        '(foo) A))))
                   $state))))))

(deftest test-cps-of-sample
  (binding [*gensym* symbol]
    (testing "cps-of-sample"
      (is (= (cps-of-sample '((foo 2)) 'ret)
             '(foo (fn arg [A $state]
                     (embang.trap/->sample 'S A ret $state))
                   $state
                   2))
          "compound sample"))))

(deftest test-cps-of-observe
  (binding [*gensym* symbol]
    (testing "cps-of-observe"
      (is (= (cps-of-observe '((normal 1 1) 2) 'ret)
             '(embang.trap/->observe 'O (normal 1 1) 2 ret $state))
          "simple observe"))))

(deftest test-mem-cps
  (binding [*gensym* symbol]
    (testing "mem-cps"
      (is (= (mem-cps '((fn [x] x)))
             '(let [M (gensym "M")]
                (fn mem [C $state & P]
                  (if (embang.state/in-mem? $state M P)
                    (fn []
                      (C (embang.state/get-mem $state M P) $state))
                    (clojure.core/apply
                      (fn fn [C $state x] (fn [] (C x $state)))
                      (fn set-mem [V $state]
                        (fn []
                          (C V (embang.state/set-mem $state
                                                     M P V))))
                      $state
                      P)))))
          "mem of compound function")
          (is (= (mem-cps '((fn foo [x] x)))
                 '(let [M (gensym "M")]
                    (fn foo [C $state & P]
                      (if (embang.state/in-mem? $state M P)
                        (fn []
                          (C (embang.state/get-mem $state M P)
                             $state))
                        (clojure.core/apply
                          (fn fn [C $state x] (fn [] (C x $state)))
                          (fn set-mem [V $state]
                            (fn []
                              (C V (embang.state/set-mem $state
                                                         M P V))))
                          $state
                          P)))))
          "mem of named compound function")
      (is (= (mem-cps '(foo))
             '(let [M (gensym "M")]
                (fn mem [C $state & P]
                  (if (embang.state/in-mem? $state M P)
                    (fn []
                      (C (embang.state/get-mem $state M P) $state))
                    (clojure.core/apply
                      foo
                      (fn set-mem [V $state]
                        (fn []
                          (C V (embang.state/set-mem $state
                                                     M P V))))
                      $state
                      P)))))
          "mem of variable"))))

(deftest test-cps-of-store
  (binding [*gensym* symbol]
    (testing "cps-of-retrieve"
      (is (= (cps-of-retrieve [:a] 'ret)
             '(fn []
                (ret (embang.state/retrieve $state :a) $state)))
          "retrieve"))
    (testing "cps-of-store"
      (is (= (cps-of-store [1] 'ret)
             '(fn []
                (ret 1 (embang.state/store $state 1))))
          "simple store")
      (is (= (cps-of-store [:a '(foo)] 'ret)
             '(foo (fn arg [A $state]
                       (fn []
                         (ret A (embang.state/store $state :a A))))
                     $state))
          "compound store"))))

(deftest test-primitive-procedure-cps
  (binding [*gensym* symbol]
    (testing "primitive-procedure-cps"
      (is (= (primitive-procedure-cps 'inc)
             '(fn inc [C $state & P]
                (fn [] (C (apply inc P) $state))))
          "primitive procedure"))))

(deftest test-cps-of-expression
  (binding [*gensym* symbol]
    (testing "(cps-of-expression simple-expression)"
      (is (= (cps-of-expression 1 'ret)
             '(fn []
                (ret 1 $state)))
          "cps of integer")
      (is (= (cps-of-expression '(if 1 2 3) 'ret)
             '(fn []
                (ret (if 1 2 3) $state)))
          "cps of simple if"))
    (testing "(cps-of-expression compound-expression)"
      (is (= (cps-of-expression '(if x (foo) (bar)) 'ret)
             '(if x
                (foo ret $state)
                (bar ret $state)))
          "cps of compound if"))))
