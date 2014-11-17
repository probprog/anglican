(ns embang.core
  (:gen-class)
  (:require [clojure.pprint :refer [pprint]])
  (:use [embang.xlat :only [program]])
  (:use [embang.trap :only [cps-of-expr]]))


;; (return (fn [C5341 $state] (C5341 nil $state)) $state)

(defn return-state [program-fn state]
  (program-fn (fn [_ state] state) state))

(defmacro anglican 
  [& source]
  `(~'fn [~'$state]
    ~(cps-of-expr (program source) return-state)))

(defn -main
  "transforms anglican program to clojure code"
  [& args]
  (pprint (cps-of-expr
           (program 
            (read-string (str "(" (slurp *in*) ")")))
           'return)))
