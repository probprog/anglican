(ns angsrc.ho
  (:use [embang emit runtime]))

;;; Examples of higher-order function use.
;;
;; This is a simple example without any
;; probabilistic reference. It shows that
;; map and reduce appear to work.
 
(defanglican ho
  [predict (map + '(1 2 3) '(1 2 3))]
  [predict (reduce + '(1 2 3))]
  [predict (reduce conj nil '(1 2 3))])
