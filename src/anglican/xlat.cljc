(ns anglican.xlat
  "Translation of Anglican Scheme to Anglican Clojure, deprecated")

;; Translator from Scheme-like legacy syntax to Clojure syntax.
;; Use of Scheme-like syntax is deprecated but supported for
;; compatibility with older programs.

(declare expression)

(defn aseq?
  "true when the argument is an Anglican sequence expression,
  both list and vector are sequences for source code
  compatibility"
  [expr]
  (or (seq? expr) (vector? expr)))

(defn elist
  "translates a list of expressions,
  replacing define with let"
  [exprs]
  (when (seq exprs)
    (lazy-seq
      (let [[expr & exprs] exprs]
        (if (and (aseq? expr) (#{'define 'assume} (first expr)))
          (let [[name value] (rest expr)]
            `((~'let [~name ~(expression value :name name)]
                ~@(elist exprs))))
          `(~(expression expr) 
            ~@(elist exprs)))))))

(defn alambda
  "translates lambda to fn,
  if name is not nil, fn is named"
  [name [parms & body]]
  `(~'fn ~@(when name [name])
     ~(if (seq? parms)
        `[~@parms]
        `[& ~parms]) ; variadic
     ~@(elist body)))

(defn amem
  "translates mem, carrying the name to the argument"
  [name [expr]]
  `(~'mem ~(expression expr :name name)))

(defn alet
  "translates let"
  [[bindings & body]]
  `(~'let [~@(mapcat (fn [[name value]]
                       [name (expression value :name name)])
                     bindings)]
     ~@(elist body)))

(defn aloop
  "translates loop+recur"
  [[bindings & body]]
  `(~(alambda 'loop `(~(map first bindings) ~@body))
    ~@(map (fn [[name value]]
             (expression value :name name))
           bindings)))

(defn acond 
  "translates cond"
  [clauses]
  `(~'cond ~@(mapcat (fn [[cnd expr]]
                       [(if (= cnd 'else) :else
                          (expression cnd))
                        (expression expr)])
                     clauses)))

(defn acase
  "translates case"
  [[key & clauses]]
  `(~'case ~(expression key)
     ~@(mapcat (fn [[tag expr :as clause]]
                 (if (= tag 'else) 
                   (expression (rest clause))
                   (expression clause)))
               clauses)))

(defn abegin
  "translates begin to do"
  [exprs]
  `(~'do ~@(elist exprs)))

(defn apredict
  "translates predict"
  ;; In Clojure representation `predict' has two arguments:
  ;; symbolic expression and value. This is necessary to
  ;; display predicted expressions in Anglican rather than
  ;; Clojure syntax.
  [args]
  (let [[label expr]
        (if (= (count args) 2)
          [(first args) (second args)]
          [`'~(first args) (first args)])]
    `(~'predict ~label ~(expression expr))))
        
(defn aform
  "translates compatible forms and function applications"
  [expr]
  (map expression expr))

(defn expression [expr & {:keys [name] :or {name nil}}]
  "translates expression"
  (if (and (aseq? expr) (seq expr))
    (let [[kwd & args] expr]
      (case kwd
        quote  expr
        lambda (alambda name args)
        let    (alet args)
        loop   (aloop args)
        mem    (amem name args)
        cond   (acond args)
        case   (acase args)
        begin  (abegin args)
        predict (apredict args)
        ;; other forms (`if', `and', `or', `application')
        ;;  have compatible structure
        (aform expr)))
    (case expr
      ;; variable names `do' and `fn' are replaced by
      ;; `begin' and `lambda' to avoid name clashes
      do 'begin
      fn 'lambda
      expr)))

(defn program
  "translates anglican program to clojure expression"
  [p]
  `(~'do ~@(elist p)))
