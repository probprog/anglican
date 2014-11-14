(ns embang.core
  (:gen-class)
  (:require [clojure.pprint :refer [pprint]])
  (:use embang.xlat))

(defn -main
  "transforms anglican program to clojure code"
  [& args]
  (pprint (anglican (read-string (str "(" (slurp *in*) ")")))))
