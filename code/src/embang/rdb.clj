(ns embang.rdb
  "Random DB, same as Lightweight Metropolis-Hastings"
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:require embang.lmh)
  (:use embang.inference))

;;;; Alias for Single-site Metropolis-Hastings

(defmethod infer :rdb [algorithm & args] (apply infer :lmh args))
