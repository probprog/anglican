(namespace embang.trap)

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

(def ^:const ^:private PRIMITIVE-FUNCTIONS
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
    conj concat])
