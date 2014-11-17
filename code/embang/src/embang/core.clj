(ns embang.core
  (:gen-class)
  (:require [clojure.pprint :refer [pprint]])
  (:use [embang.emit :only [anglican->fn]]))

(defn -main
  "transforms anglican program to clojure code"
  [& args]
  (pprint (anglican->fn
           (read-string (str "(" (slurp *in*) ")")))))
