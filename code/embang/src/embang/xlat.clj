(ns embang.xlat)

(declare expression)

(defn elist
  "translates a list of expressions,
  replacing define with let"
  [exprs]
  (when (seq exprs)
    (lazy-seq
      (let [[expr & exprs] exprs]
        (if (and (seq? expr) (= (first expr) 'define))
          (let [[name value] (rest expr)]
            `((~'let [~name ~(expression value)]
                ~@(elist exprs))))
          (cons (expression expr) (elist exprs)))))))

(defn alambda
  "translates lambda to fn"
  [[parms & body]]
  `(~'fn
     ~(if (list? parms)
        `[~@parms]
        `[& ~parms])
     ~@(elist body)))

(defn alet
  "translates let"
  [[bindings & body]]
  `(~'let [~@(mapcat (fn [[name value]]
                     [name (expression value)])
                   bindings)]
     ~@(elist body)))

(defn acond 
  "translates cond to nested ifs"
  [[& clauses :as args]]
  `(~'cond ~@(mapcat (fn [[cnd expr]]
                       [(if (= cnd 'else) :else
                          (expression cnd))
                        (expression expr)])
                     clauses)))

(defn abegin
  "translates begin to do"
  [[& body]]
  `(~'do ~@(elist body)))

(defn aform
  "translates compatible forms and function applications"
  [expr]
  (map expression expr))

(defn expression [expr]
  "translates expression"
  (if (list? expr)
    (if (seq expr)
      (let [[kwd & args] expr]
        (case kwd
          quote  expr
          lambda (alambda args)
          let    (alet args)
          cond   (acond args)
          begin  (abegin args)
          ;; other forms (if, and, or, application) have  compatible structure
          (aform expr)))
      ()) ; anglican allows unquoted empty list
    expr))

(defn dlist
  "translates directive list, replacing assume with let"
  [ds]
  (when (seq ds)
    (lazy-seq
      (let [[[kwd & args :as d] & ds] ds]
        (case kwd
          assume (let [[name value] args]
                   `((~'let [~name ~(expression value)]
                       ~@(dlist ds))))
          (observe predict) (cons `(~kwd ~@(map expression args))
                                  (dlist ds))
          (assert false (str "unrecognized directive: " d)))))))

(defn program
  "translates anglican program into clojure function"
  [& p]
  `(~'fn []
     ~@(dlist p)))
