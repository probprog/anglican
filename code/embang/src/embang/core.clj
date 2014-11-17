(ns embang.core
  (:gen-class)
  (:require [clojure.pprint :refer [pprint]])
  (:use [embang.xlat :only [anglican]])
  (:use [embang.trap :only [cps-of-expr]]))

(defn -main
  "transforms anglican program to clojure code"
  [& args]
  (pprint (cps-of-expr
           (anglican 
            (read-string (str "(" (slurp *in*) ")")))
           'return)))
