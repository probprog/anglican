(ns embang.core
  (:gen-class)
  (:require [clojure.pprint :refer [pprint]])
  (:use [embang.xlat :only [anglican]]
        [embang.trap :only [simplify]]))

(defn -main
  "transforms anglican program to clojure code"
  [& args]
  (pprint (simplify 
           (anglican 
            (read-string (str "(" (slurp *in*) ")"))))))
