(ns anglican.rdb
  "Random DB, same as Lightweight Metropolis-Hastings"
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:require anglican.lmh)
  (:use #?(:clj anglican.inference
          :cljs [anglican.inference :only [infer]])))

;;;; Alias for Single-site Metropolis-Hastings

(defmethod infer :rdb [algorithm & args] (apply infer :lmh args))
