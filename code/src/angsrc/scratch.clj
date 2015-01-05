(ns angsrc.scratch
  (:use [embang emit runtime]))

;;; Scratch pad

(defanglican scratch
  [assume fact (lambda (n)
                  (loop ((n n) (acc 1))
                    (if (<= n 1) acc
                        (recur (- n 1) (* acc n)))))]
  [predict (map fact (range 10))])
