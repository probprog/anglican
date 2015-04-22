(ns fib
  (:use [anglican runtime emit]))

(defquery fib 
  "computes arbitrarily large fibonacci numbers,
  as large as the virtual memory could accomodate"
  n ; this is the argument, use -v on the command line
  (let [fib (mem (fn fib* [n]
                   (case n
                     1 1N
                     2 1N
                     (+ (fib* (- n 2)) (fib* (- n 1))))))]
    (predict (fib n))))
