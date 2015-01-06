(ns embang.emit
  (:use [embang.xlat :only [program alambda]])
  (:use [embang.trap :only [cps-of-expression run-cont 
                            fn-cps primitive-procedure-cps]]))

;;;; Top-level forms for anglican programs

;;; Code manipulation

(defn ^:private overriding-higher-order-functions
  "binds names of higher-order functions
  to their CPS implementations"
  [& body]
  `(~'let [~@(mapcat (fn [fun] [fun (symbol (str "$" fun))]) 
                     '[map reduce
                       filter some])]
     ~@body))

(defmacro anglican 
  "macro for embedding anglican programs"
  [& source]
  (overriding-higher-order-functions
   `(~'fn [~'_ ~'$state]
      ~(cps-of-expression (program source) run-cont))))

(defmacro defanglican
  "binds variable to anglican program"
  [name & args]
  (let [[docstring source]
        (if (string? (first args))
          [(first args) (rest args)]
          [(format "anglican program '%s'" name) args])]
    `(def ~(with-meta name {:doc docstring})
       (anglican ~@source))))

;;; Auxiliary macros

;; When a function is defined outside an Anglican 
;; program, it must be in CPS form. cps-fn and def-cps-fn
;; are like fn and defn but automatically transform functions
;; into CPS.
;;
;; $map and $reduce are not rebound as map
;; and reduce because def-cps-fn are used to
;; define the CPS versions.

(defmacro cps-fn
  "converts function to CPS,
  useful for defining functions outside of defanglican"
  [& args]
  (fn-cps args))

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

(defmacro lambda
  "defines function in Anglican syntax"
  [& args]
  (overriding-higher-order-functions
   `(cps-fn ~@(next (alambda nil args)))))

(defmacro defun
  "binds variable to function in Anglican syntax"
  [name & args]
  (overriding-higher-order-functions
   `(def-cps-fn ~@(next (alambda name args)))))

;; Any non-CPS procedures can be used in the code,
;; but must be wrapped and re-bound.

(defmacro
  with-primitive-procedures
  "binds primitive procedure names to their CPS versions"
  [procedures & body]
  `(let [~@(mapcat (fn [proc] 
                     [proc (primitive-procedure-cps proc)])
                   procedures)]
     ~@body))

;;;; Program prologue

;; Common anglican code that must be available to all
;; programs goes here. 

;;; CPS versions of higher-order functions

;; Higher-order functions must be re-implemented in CPS,
;; such that both the functions themselves and the
;; functional arguments follow the CPS calling convention.
;; They are rebound in macros defining anglican code by
;; calling `overriding-higher-order-functions'.

(def-cps-fn ^:private $map1 
  "map on a single sequence"
  [fun lst]
  (if (empty? lst) nil
      (cons (fun (first lst))
            ($map1 fun (rest lst)))))

(def-cps-fn ^:private $nils? 
  "true if the list contains nil"
  [lst]
  (and (seq lst)
       (or (nil? (first lst))
           ($nils? (rest lst)))))

(def-cps-fn $map 
  "map in CPS"
  [fun & lsts]
  (let [tuple ($map1 first lsts)]
    (if ($nils? tuple) nil
        (let [lsts ($map1 rest lsts)]
          (cons (apply fun tuple)
                (apply $map fun lsts))))))

(def-cps-fn ^:private $reduce1
  "reduce with explicit init in CPS"
  [fun init lst]
  (if (empty? lst) init
      ($reduce1 fun
                (fun init (first lst))
                (rest lst))))

(def-cps-fn $reduce
  "reduce in CPS"
  [fun & args]
  (let [init (if (seq (rest args))
               (first args) 
               (first (first args)))
        lst (if (seq (rest args))
              (second args)
              (rest (first args)))]
    ($reduce1 fun init lst)))

(def-cps-fn $filter
  "filter in CPS"
  [fun lst]
  (cond
   (empty? lst) lst
   (fun (first lst)) (cons (first lst) ($filter fun (rest lst)))
   :else ($filter fun (rest lst))))

(def-cps-fn $some
  "some in CPS"
  [fun lst]
  (when (seq lst)
    (or (fun (first lst))
        ($some fun (rest lst)))))
