(ns embang.emit
  (:use [embang.xlat :only [program]])
  (:use [embang.trap :only [cps-of-expr run-cont]]))

;;; Code manipulation

(defn anglican->fn
  "converts anglican source code to a
  trampoline-ready clojure function"
  [source]
  `(~'fn [~'_]
     ~(cps-of-expr (program source) `run-cont)))

(defmacro anglican 
  "macro for embedding anglican programs"
  [& source]
  (anglican->fn source))
