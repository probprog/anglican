(ns angsrc.scratch
  (:use [embang emit runtime]))

;;; Scratch pad

(def-cps-fn foo []
  (if 1 2 3
  (predict (/ 1 0))))

(defquery scratch
  (let [x 1
        y 2]
    (predict x)
    (predict y)
    (predict 'sum (+ x y))
    (foo)))
