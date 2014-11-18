(ns embang.emit
  (:use [embang.xlat :only [program]])
  (:use [embang.trap :only [cps-of-expr result-cont map->state]]))

(defn anglican->fn
  "converts anglican source code to a
  trampoline-ready clojure function"
  [source]
  `(~'fn [~'_ ~'$state]
     ~(cps-of-expr (program source) `result-cont)))

(defmacro anglican 
  "macro for embedding anglican programs"
  [& source]
  (anglican->fn source))

(defn run-step
  "a wrapper that runs the program until the
  next checkpoint (one of sample, observe, mem, result)"
  ([prog value state] ; any intermediate step
    (trampoline prog value state))
  ([prog]             ; first step
   (prog nil (map->state {:log-weight 1.
                          :predicts []}))))
