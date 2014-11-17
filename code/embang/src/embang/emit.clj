(ns embang.emit
  (:use [embang.xlat :only [program]])
  (:use [embang.trap :only [cps-of-expr result-cont]]))

(defn anglican->fn
  "converts anglican source code to a
  trampoline-ready clojure function"
  [source]
  `(~'fn [~'$state]
     ~(cps-of-expr (program source) `result-cont)))

(defmacro anglican 
  "macro for embedding anglican programs"
  [& source]
  (anglican->fn source))
