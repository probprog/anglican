(ns embang.trap)

;;; Trampoline-ready Anglican program 

;; The input to this series of transformation is an Anglican program
;; in clojure syntax (embang.xlat). The output is a Clojure function
;; that returns either the next step as a structure incroprorating
;; continuations and parameters, or the result containing a vector of
;; predicted values and the sample weight.
;;
;; Steps are delimited by random choices.  Between the steps, the
;; inference decisions can be made.
;;
;; The state is threaded through computation and consists of
;;   - the running sample weight
;;   - the list of predicted values.

(declare ^:dynamic *primitive-procedures*)

;; Continuation access --- value and state

(defn value-cont [v _] v)
(defn state-cont [_ s] s)

;; State updating, predicts and weights

;; TODO

;; Interrupt points

(defrecord observe [id cont state])
(defrecord sample [id dist cont state])
(defrecord mem [id args proc cont])
(defrecord result [state])

;; Retrieval of final result:
;; run the function on the initial state
;; and return the final state wrapped into `result'.

(defn result-cont [f s] 
  (->result (f state-cont s)))

;; Running state

(defrecord state [log-weight predicts])

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

;; CPS transformation rules

(declare cps-of-expr)
(def ^:dynamic *gensym* gensym)

(defn cps-of-elist
  [exprs cont]
  (let [[fst & rst] exprs]
    (if (seq rst)
      (if (simple-expr? fst)
        (cps-of-elist rst cont)
        (cps-of-expr fst
                     `(~'fn [~'_]
                        ~(cps-of-elist rst cont))))
      (cps-of-expr fst cont))))

;; Asserts in cps-of-fn, cps-of-let make sure primitive procedures are
;; uniquely identifiable by their names.

;; Continuation is the first, rather than the last, parameter of a
;; function to support functions with variable arguments.

(defn cps-of-fn
  [args cont]
  (if (vector? (first args))
    (cps-of-fn `[nil ~@args] cont)
    (let [[name parms & body] args
          fncont (*gensym* "C")]
      (assert (not-any? primitive-procedure? parms)
              (str "primitive procedure name as parameter: "
                   (some primitive-procedure? parms)))
      `(~cont (~'fn ~@(when name [name])
                [~fncont ~'$state ~@parms]
                ~(cps-of-elist body fncont))
              ~'$state))))

(defn cps-of-let
  "transforms let to cps"
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
                     (let [value (*gensym* "V")]
                       `(~'fn [~value ~'$state]
                          (~'let [~name ~value]
                            ~rest))))))
    (cps-of-elist body cont)))

(defmacro defn-with-named-cont 
  "binds the continuation to a name to make the code
  slightly easier to reason about"
  [cps-of parms & body]
  (let [cont (last parms)]
    `(defn ~cps-of ~parms
       (if (symbol? ~cont)
         (do ~@body)
         (let [~'named-cont (*gensym* "C")]
           `(~~''let [~~'named-cont ~~cont]
              ~(~cps-of ~@(butlast parms) ~'named-cont)))))))

(defn-with-named-cont
  ^{:doc "transforms if to cps"}
  cps-of-if
  [[cnd thn els] cont]
  (if (simple-expr? cnd)
      `(~'if ~cnd
         ~(cps-of-expr thn cont)
         ~(cps-of-expr els cont))
      (cps-of-expr cnd
                   (let [cnd (*gensym* "I")]
                     `(~'fn [~cnd ~'$state]
                        (if ~cnd
                          ~(cps-of-expr thn cont)
                          ~(cps-of-expr els cont)))))))
  
(defn-with-named-cont 
  ^{:doc "transforms cond to cps"}
  cps-of-cond
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
  predict appends predicted expression
  and its value to :predicts"
  [[pred] cont]
  `(~'let [~'$state (update-in ~'$state
                          [:predicts] conj ['~pred ~pred])]
    (~cont nil ~'$state)))

(declare cps-of-application)

(defn cps-of-observe
  "transforms observe to cps,
  observe updates the weight by adding
  the result of observe (log-probability)
  to the log-weight"
  [args cont]
  (cps-of-application 
    `(~'observe ~@args) 
    (let [id (*gensym* "O")
          lw (*gensym* "L")]
      `(~'fn [~lw ~'$state]
         (~'let [~'$state (update-in ~'$state
                                     [:log-weight] + ~lw)]
           (->observe '~id ~cont ~'$state))))))

(defn cps-of-sample
  "transforms sample to cps,
  on sample the program is interrupted
  and the control is transferred to the inference
  algorithm"
  [[dist] cont]
  (let [id (*gensym* "S")
        dname (*gensym* "D")]
    (cps-of-expr dist
                 `(~'fn [~dname ~'$state]
                    (->sample '~id ~dname ~cont ~'$state)))))

(defn cps-of-mem
  "transforms mem to cps"
  [[[_ & args]] cont]
  (let [id (*gensym* "M")
        mcont (*gensym* "C")
        mparms (*gensym* "P")
        [name parms & body] (if (vector? (first args))
                              `(nil ~@args)
                              args)]
    `(~cont (~'fn ~@(when name [name])
              [~mcont ~'$state & ~mparms]
              (->mem '~id ~mparms
                     ~(cps-of-fn `(~parms ~@body) `value-cont)
                     ~mcont))
            ~'$state)))

(defn cps-of-application
  "transforms application to cps;
  application of user-defined (not primitive) procedures
  are trampolined --- wrapped into a parameterless closure"
  [exprs cont]
  (let [args (map (fn [expr]
                    (if (simple-expr? expr)
                      [nil expr]
                      [expr (*gensym* "A")]))
                  exprs)]
    (letfn [(cps-of-alist [alist]
              (if (seq alist)
                (let [[[expr subst] & alist] alist]
                  (if expr
                    (cps-of-expr expr `(~'fn [~subst]
                                         ~(cps-of-alist alist)))
                    (cps-of-alist alist)))
                (let [call (map second args)
                      rator (first call)
                      rands (rest call)]
                  (if (primitive-procedure? rator)
                    `(~cont ~call ~'$state)
                    `(~'fn [] (~rator ~cont ~'$state ~@rands))))))]
      (cps-of-alist args))))

(defn cps-of-expr
  [expr cont]
  (cond
     (nil? expr) `(~cont ~expr ~'$state)
     (seq? expr) 
     (let [[kwd & args] expr]
        (case kwd
          quote   `(~cont ~expr ~'$state)
          fn      (cps-of-fn args cont)
          let     (cps-of-let args cont)
          if      (cps-of-if args cont)
          cond    (cps-of-cond args cont)
          do      (cps-of-do args cont)
          predict (cps-of-predict args cont)
          observe (cps-of-observe args cont)
          sample  (cps-of-sample args cont)
          mem     (cps-of-mem args cont)
          ;; application
          (cps-of-application expr cont)))
     :else `(~cont ~expr ~'$state)))

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

     ;; distribution methods
     observe sample

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
