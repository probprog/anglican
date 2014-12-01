(ns angsrc.scratch
  (:use [embang emit runtime]))

;;; Scratch pad

(defanglican scratch
  [assume ops (list inc dec)]
  [predict ((first ops) 1)]
  [predict ((second ops) 1)])
