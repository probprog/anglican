(ns angsrc.branching
  (:use [embang emit runtime]))

;;; Simple branching

(defanglican branching
  "simple branching"
  [assume fib (lambda (n)
                      (cond ((= n 0) 0)
                            ((= n 1) 1)
                            (else (+ (fib (- n 1))
                                     (fib (- n 2))))))]
  [assume r (sample (poisson 4))]
  [assume l (if (< 4 r)
              6
              (+ (fib (* 3 r)) (sample (poisson 4))))]
  [observe (poisson l) 6]
  [predict r])

;; Functions can be defined outside of the program
;; body, and used in multiple Anglican programs.

(def-lambda afib (n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (afib (- n 1))
                 (afib (- n 2))))))

(defanglican sans-fib
  "simple branching with fib externally defined"
  [assume r (sample (poisson 4))]
  [assume l (if (< 4 r)
              6
              (+ (afib (* 3 r)) (sample (poisson 4))))]
  [observe (poisson l) 6]
  [predict r])

;; Externally defined functions can also be 
;; written in Clojure.

(def-cps-fn cfib [n]
  (cond (= n 0) 0
        (= n 1) 1
        :else (+ (cfib (- n 1))
                 (cfib (- n 2)))))

(defanglican clojure-fib
  "simple branching with fib implemented in Clojure"
  [assume r (sample (poisson 4))]
  [assume l (if (< 4 r)
              6
              (+ (cfib (* 3 r)) (sample (poisson 4))))]
  [observe (poisson l) 6]
  [predict r])
