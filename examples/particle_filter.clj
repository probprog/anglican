(ns particle-filter
  (:use [embang runtime emit]))

;; Example to run with pfilter.

(let [counter (atom 0)]
  (defn make-id []
    (swap! counter inc)))

(with-primitive-procedures [make-id]
  (defquery particle-filter
    "filters particles infinitely"
    (let [_ (sample (flip 0.5)) ; stop the warmup
          id (make-id)]
      (predict :prior id)
      ((fn loop []
         ;; the longer it runs with pfilter, the
         ;; fewer particles with id>1 survive.
         (observe (flip (/ id)) true)
         (predict :posterior id)
         (loop))))))
