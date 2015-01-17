(ns embang.rdb
  (:use embang.inference))

;;;; Alias for Single-site Metropolis-Hastings

(defn infer :rdb [& args] (apply infer :smh args))
