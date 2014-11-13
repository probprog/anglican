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
            `((let [~name ~(expression value)]
                ~@(elist exprs))))
          (cons (expression expr) (elist exprs)))))))

(defn alambda
  "translates lambda to fn"
  [[parms & body]]
  `(fn
     ~(if (list? parms)
        `[~@parms]
        `[& ~parms])
     ~@(elist body)))

(defn alet
  "translates let"
  [[bindings & body]]
  `(let [~@(mapcat (fn [[name value]]
                     [name (expression value)])
                   bindings)]
     ~@(elist body)))

(defn aif
  "translates if to its three-argument form"
  [[cnd thn els :as args]]
  (assert (#{2 3} (count args)) (str "illegal if: " `(~'if ~@args)))
  `(~'if ~(expression cnd) ~(expression thn) ~(expression els)))

(defn acond 
  "translates cond to nested ifs"
  [[& clauses :as args]]
  (when (seq clauses)
    (let [[[cnd expr] & clauses] clauses]
      (if (= cnd 'else)
        (do
          (assert (empty? clauses)
                  (str "else clause must be last: " `(~'cond ... ~@args)))
          (expression expr))
        `(~'if ~(expression cnd) ~(expression expr)
           ~(acond clauses))))))

(defn abegin
  "translates begin to do"
  [[& body]]
  `(do ~@(elist body)))

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
          if     (aif args)
          cond   (acond args)
          begin  (abegin args)
          ;; other forms (and, or, application) have  compatible structure
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
                   `((let [~name ~(expression value)]
                       ~@(dlist ds))))
          observe (cons `(~'observe ~@(map expression args)) (dlist ds))
          predict (cons `(~'predict ~@(map expression args)) (dlist ds))
          (assert false (str "unrecognized directive: " d)))))))

(defn program
  "translates anglican program into clojure function"
  [& p]
  `(fn []
     ~@(dlist p)))
