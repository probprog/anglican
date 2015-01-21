(ns embang.rdb
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:use embang.inference))

;;;; Alias for Single-site Metropolis-Hastings

(defmethod infer :rdb [algorithm & args] (apply infer :lmh args))
