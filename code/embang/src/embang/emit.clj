(ns embang.emit
  (:use [embang.xlat :only [program alambda]])
  (:use [embang.trap :only [cps-of-expr run-cont 
                            cps-of-fn value-cont]]))

;;; Code manipulation

(defn anglican->fn
  "converts anglican source code to a
  trampoline-ready clojure function"
  [source]
  `(~'fn [~'_ ~'$state]
     ~(cps-of-expr (program source) `run-cont)))

(defmacro anglican 
  "macro for embedding anglican programs"
  [& source]
  (anglican->fn source))

(defmacro defanglican
  "binds variable to anglican program"
  [name & args]
  (let [[docstring source]
        (if (string? (first args))
          [(first args) (rest args)]
          [(format "anglican program '%s'" name) args])]
    `(def ~(with-meta name {:doc docstring}) (anglican ~@source))))

;;; Auxiliary macros

;; When a function is defined outside an Anglican 
;; program, it must be in CPS form. cps-fn and def-cps-fn
;; are like fn and defn but automatically transform functions
;; into CPS.

(defmacro cps-fn
  "converts functional value to CPS,
  useful for defining functions outside of defanglican"
  [& args]
  `(~'let [~'$state nil]
    ~(cps-of-fn args value-cont)))
  
(defmacro def-cps-fn
  "binds variable to function in CPS form"
  [name & args]
  (let [[docstring source]
        (if (string? (first args))
          [(first args) (rest args)]
          [(format "CPS function '%s'" name) args])
        arglist (first source)]
    `(def ~(with-meta name
                      {:doc docstring
                       :arglists `'([~'$cont ~'$state
                                     ~@(first source)])})
       (cps-fn ~name ~@source))))

;; Functions can also be defined in Anglican rather
;; than Clojure syntax. 

(defmacro def-lambda 
  "defines variable in clojure syntax"
  [name & args]
  `(def-cps-fn ~@(next (alambda name args))))
