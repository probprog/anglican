(ns embang.core
  (:gen-class)
  (:require [clojure.pprint :refer [pprint]])
  (:use embang.xlat))

(defn -main
  "transforms anglican to clojure"
  [& args]
  (pprint (apply program (read-string (str "(" (slurp *in*) ")")))))
