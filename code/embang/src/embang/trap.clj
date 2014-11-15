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

(

(declare cps-of-expr)

(declare cps-of-elist)

(defn simple-expr?
  [expr]
  false)

(defn cps-of-fn
  [args cont]
  (if (vector? (first args))
    (cps-of-fn `[nil ~@args] cont)
    (let [[name parms & body] args
          fncont (gensym "cont")]
      `(~cont (~'fn ~@(when name [name])
                [~fncont ~parms]
                ~(cps-of-expr body fncont))))))

(defn cps-of-if
  "transforms cond to cps"
  [[cnd thn els] cont]
  (if (simple-expr? cnd)
    `(~'if ~cnd
       ~(cps-of-expr thn cont)
       ~(cps-of-expr els cont))
    (let [pcnd (gensym "cnd")]
      (cps-of-expr cnd `(~'fn [~pcnd]
                          (if ~pcnd
                            ~(cps-of-expr thn cont)
                            ~(cps-of-expr els cont)))))))
                          
(defn cps-of-cond
  "transforms cond to cps"
  [clauses cont]
  (if clauses
    (let [[cnd thn & clauses] clauses]
      (cps-of-if [cnd thn `(~'cond ~@clauses)] cont))
    (cps-of-expr nil cont)))

(defn cps-of-do
  "transforms do to cps"
  [exprs cont]
  `(~cont ~@exprs))

(defn cps-of-expr
  [expr cont]
  (cond
     (nil? expr) `(~cont ~expr)
     (seq? expr) 
     (let [[kwd & args] expr]
        (case kwd
          quote   `(~cont ~expr)
          mem     `(~cont ~expr)
          fn      (cps-of-fn args cont)
          let     `(~cont ~expr)
          if      (cps-of-if args cont)
          cond    (cps-of-cond args cont)
          do      `(~cont ~expr)
          predict `(~cont ~expr)
          observe `(~cont ~expr)
          ;; application
          `(~cont ~expr)))
     :else `(~cont ~expr)))

(def ^:const ^:private PRIMITIVE-FUNCTIONS
  "primitive functions, do not exist in CPS form"
  '[ ;; tests
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
    normal-with-known-std])
