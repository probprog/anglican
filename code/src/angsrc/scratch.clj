(ns angsrc.scratch
  (:use [embang emit runtime]))

;;; Scratch pad

(defanglican scratch
  (store :a 1)
  (store :b 2)
  [predict (retrieve :a)]
  [predict (retrieve :b)]
  (store :a 3)
  [predict (retrieve :a)]
  [predict (retrieve :c)])
