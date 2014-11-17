(ns embang.trap)

;;; Trampoline-ready Anglican program 

;; The input to this series of transformation is an Anglican
;; program in clojure syntax (embang.xlat). The output is a
;; Clojure function that returns either the next step as a
;; continuation parameterized by the random choice, or the
;; result as a vector of predicted values and the sample weight.
;;
;; Steps are delimited by random choices.  Between the steps,
;; the inference decisions can be made.
;;
;; The state is threaded through computation and consists of
;;   - the running sample weight
;;   - the list of predicted values.

(declare cps-of-expr)
(declare ^:dynamic *primitive-procedures*)

(defn primitive-procedure?
  "true if the procedure is primitive,
  that is does not have a CPS form"
  ;; assumes that primitive procedure
  ;; symbols are never rebound locally
  [procedure]
  (*primitive-procedures* procedure))

(defn simple-expr?
  "true if expr has no continuation"
  [expr]
  (or (nil? expr)
      (not (seq? expr))
      (case (first expr)
        quote true
        (begin if cond do) (every? simple-expr? (rest expr))
        let (let [[_ bindings & body] expr]
              (and (every? simple-expr? (map second bindings))
                   (every? simple-expr? body)))
        (and (primitive-procedure? (first expr))
             (every? simple-expr? (rest expr))))))

(defn cps-of-elist
  [exprs cont]
  (let [[fst & rst] exprs]
    (if (seq rst)
      (cps-of-expr fst
                   `(~'fn [~'_]
                      ~(cps-of-elist rst cont)))
      (cps-of-expr fst cont))))

;; Asserts in cps-of-fn, cps-of-let make sure
;; primitive procedures are uniquely identifiable by
;; their names. 

;; Continuation is the first, rather than the last, parameter
;; of a function to support functions with variable arguments.

(defn cps-of-fn
  [args cont]
  (if (vector? (first args))
    (cps-of-fn `[nil ~@args] cont)
    (let [[name parms & body] args
          fncont (gensym "C")]
      (assert (not-any? primitive-procedure? parms)
              (str "primitive procedure name as parameter: "
                   (some primitive-procedure? parms)))
      `(~cont (~'fn ~@(when name [name])
                [~fncont ~@parms]
                ~(cps-of-elist body fncont))))))

(defn cps-of-let
  "transforms let tpo cps"
  [[bindings & body] cont]
  (if (seq bindings)
    (let [[name value & bindings] bindings
          rest (cps-of-let `(~bindings ~@body) cont)]
      (assert (not (primitive-procedure? name))
              (str "primitive procedure name rebound: " name))
      (assert (not (primitive-procedure? value))
              (str "primitive procedure locally bound: " value))
      (if (simple-expr? value)
        `(~'let [~name ~value]
           ~rest)
        (cps-of-expr value
                     (let [value (gensym "V")]
                       `(~'fn [~value]
                          (~'let [~name ~value]
                            ~rest))))))
    (cps-of-elist body cont)))

(defmacro defn-with-named-cont 
  [cps-of parms & body]
  (let [cont (last parms)]
    `(defn ~cps-of ~parms
       (if (symbol? ~cont)
         (do ~@body)
         (let [~'named-cont (gensym "C")]
           `(~~''let [~~'named-cont ~~cont]
              ~(~cps-of ~@(butlast parms) ~'named-cont)))))))

(defn-with-named-cont cps-of-if
  [[cnd thn els :as args] cont]
  (if (simple-expr? cnd)
      `(~'if ~cnd
         ~(cps-of-expr thn cont)
         ~(cps-of-expr els cont))
      (cps-of-expr cnd
                   (let [cnd (gensym "I")]
                     `(~'fn [~cnd]
                        (if ~cnd
                          ~(cps-of-expr thn cont)
                          ~(cps-of-expr els cont)))))))
  
(defn-with-named-cont cps-of-cond
  [clauses cont]
  (if clauses
    (let [[cnd thn & clauses] clauses]
      (cps-of-if [cnd thn `(~'cond ~@clauses)] cont))
    (cps-of-expr nil cont)))

(defn cps-of-do
  "transforms do to cps"
  [exprs cont]
  `(cps-of-elist exprs cont))

(defn cps-of-predict
  "transforms predict to cps,
  predict appends current expression
  and its value to $predicts"
  [[expr] cont]
  `(~'let [~'$predicts (~'conj ~'$predicts ['~expr ~expr])]
     (~cont nil)))


(defn cps-of-alist
  "cps of expanded argument list,  each entry in
   the argument list  is a pair [expression substitution],
   when expression is nil, it is simple and does not require
   a continuation"
  [alist])

  
(defn cps-of-application
  "transforms application to cps"
  [exprs cont]
  (let [args (map (fn [expr]
                    (if (simple-expr? expr) [nil expr] [expr (gensym "A")]))
                  exprs)]
    (letfn [(cps-of-alist [alist]
              (if (seq alist)
                (let [[[expr subst] & alist] alist]
                  (if expr
                    (cps-of-expr expr `(~'fn [~subst]
                                         ~(cps-of-alist alist)))
                    (cps-of-alist alist)))
                (let [call (map second args)]
                  (if (primitive-procedure? (first call))
                    `(~cont ~call)
                    `(~(first call) ~cont ~@(rest call))))))]
      (cps-of-alist args))))

(defn cps-of-expr
  [expr cont]
  (cond
     (nil? expr) `(~cont ~expr)
     (seq? expr) 
     (let [[kwd & args] expr]
        (case kwd
          quote   `(~cont ~expr)
          fn      (cps-of-fn args cont)
          let     (cps-of-let args cont)
          if      (cps-of-if args cont)
          cond    (cps-of-cond args cont)
          do      (cps-of-do args cont)
          predict (cps-of-predict args cont)
          observe `(~'TODO ~cont ~expr)
          sample  `(~'TODO ~cont ~expr)
          mem     `(~'TODO ~cont ~expr)
          ;; application
          (cps-of-application expr cont)))
     :else `(~cont ~expr)))

(def ^:dynamic *primitive-procedures*
  "primitive procedures, do not exist in CPS form"
  '#{ ;; tests
    boolean? symbol? string?   proc? number?
    ratio?  integer?  float?  even?  odd?
    nil?  some?  empty?  list?  seq?  

    ;; custom math tests
    isfinite?  isnan?

    ;; relational
    not= = > >= < <=

    ;; scalar arithmetics
    inc dec
    + - * / mod
    abs floor ceil round
    sin cos tan asin acos atan
    sinh cosh tanh
    log log10 exp
    pow cbrt sqrt
    
    ;; sequence operations
    sum cumsum mean normalize range

    ;; casting
    boolean double long read-string str

    ;; data structures – documented
    list first second nth rest count
    conj concat

    ;; higher-order functions
    map reduce apply mem

    ;; ERPs
    beta
    binomial
    categorical
    dirac
    dirichlet
    discrete
    discrete-cdf
    exponential
    flip
    gamma
    normal
    mvn
    wishart
    poisson
    uniform-continuous
    uniform-discrete

    ;; XRPs
    crp
    beta-flip
    normal-with-known-std})
