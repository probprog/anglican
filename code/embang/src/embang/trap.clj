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

(declare simplify)

(defn simplify-elist
  "simplifies expression list"
  [elist]
  (let [[e & elist] elist]
    (if elist
      `((~'fn [~'_]
          ~(simplify-elist elist))
        ~(simplify e))
      (simplify e))))

(defn simplify-fn
  "simplifies function by
  eliminating expression list in the body"
  [args]
  (if (symbol? (first args)) ; named fn
    (let [[name parms & body] args]
      `(~'fn ~name ~parms ~(simplify-elist body)))
    (let [[parms & body] args]
      `(~'fn ~parms ~(simplify-elist body)))))

(defn simplify-let
  "replaces let with application of fn"
  [[bindings & body]]
  (let [bindings (partition 2 bindings)]
    `(~(simplify `(~'fn ~(vec (map first bindings)) ~@body))
      ~@(map (comp simplify second) bindings))))

(defn simplify-if
  "replaces if with cond"
  [[cnd thn els]]
  (simplify 
   `(~'cond ~cnd ~thn
            ~@(when els `(:else ~els)))))

(defn simplify-cond
  "simplifies expressions in cond recursively"
  [args]
  `(~'cond ~@(map simplify args)))

(defn simplify-do
  "eliminates do"
  [args]
  (simplify-elist args))

(defn simplify
  "simplifies clojure representation of an anglican program:
  `if' is replaced with `cond',
  `let' is replaced with application of `fn',
  sequences are represented by nested `fn'"
  [code]
  (if (seq? code)
    (let [[kwd & args] code]
      (case kwd
        quote code
        fn    (simplify-fn args)
        let   (simplify-let args)
        if    (simplify-if args)
        cond  (simplify-cond args)
        do    (simplify-do args)
        (map simplify code)))
    code))

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
