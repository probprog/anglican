(ns particle-filter
  (:use [embang runtime emit]))

;; Example to run with pfilter.

(let [counter (atom 0)]
  (defn make-id []
    (swap! counter inc)))

(with-primitive-procedures [make-id]
  (defquery particle-filter
    "filters particles infinitely"
    (let [chance (sample (flip 0.5))
          id (make-id)]
      ((fn loop [p]
         (predict (list :prior id)  chance)
         (observe (flip p) chance)
         (predict (list :posterior id) chance)
         (loop (- 1 p)))
       (/ 1. 3.)))))
  


