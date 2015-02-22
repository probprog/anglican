(ns particle-filter
  (:use [embang runtime emit]))

;; Example to run with pfilter.

(let [counter (atom 0)]
  (defn make-id []
    (swap! counter inc)))

(with-primitive-procedures [make-id]
  (defquery particle-filter
    "filters particles infinitely"
    (let [_ (sample (flip 0.5))
          id (make-id)]
      (predict :prior id)
      ((fn loop []
         (observe (flip (/ id)) true)
         (predict :posterior id)
         (loop))))))

  


