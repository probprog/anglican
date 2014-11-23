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
          `(~(expression expr) 
            ~@(elist exprs)))))))

(defn alambda
  "translates lambda to fn,
  if name is not nil, fn is named"
  [name [parms & body]]
  `(~'fn ~@(when name [name])
     ~(if (list? parms)
        `[~@parms]
        `[& ~parms])
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

(defn acond 
  "translates cond to nested ifs"
  [clauses]
  `(~'cond ~@(mapcat (fn [[cnd expr]]
                       [(if (= cnd 'else) :else
                          (expression cnd))
                        (expression expr)])
                     clauses)))

(defn abegin
  "translates begin to do"
  [exprs]
  `(~'do ~@(elist exprs)))

(defn aform
  "translates compatible forms and function applications"
  [expr]
  (map expression expr))

(defn expression [expr & {:keys [name] :or {name nil}}]
  "translates expression"
  (if (seq? expr)
    (let [[kwd & args] expr]
      (case kwd
        quote  expr
        lambda (alambda name args)
        let    (alet args)
        mem    (amem name args)
        cond   (acond args)
        begin  (abegin args)
        ;; other forms (if, and, or, application) have  compatible structure
        (aform expr)))
    (case expr
      ;; replace variable names `do' and `fn' by `begin' and `lambda',
      ;; to avoid name clashes in Clojure
      do 'begin
      fn 'lambda
      expr)))

(defn dlist
  "translates directive list, replacing assume with let"
  [ds]
  (when (seq ds)
    (lazy-seq
      (let [[[kwd & args :as d] & ds] ds]
        (case kwd
          assume (let [[name value] args]
                   `((~'let [~name ~(expression value :name name)]
                       ~@(dlist ds))))
          observe `((~'observe ~@(map expression args))
                    ~@(dlist ds))
          predict (let [[expr] args]
                     `((~'predict '~expr ~(expression expr))
                       ~@(dlist ds)))
          (assert false (str "unrecognized directive: " d)))))))

(defn program
  "translates anglican program to clojure function"
  [p]
  `(~'fn []
     ~@(dlist p)))
