(ns embang.trap
  (:use embang.state))

;;; Trampoline-ready Anglican program

;; The input to this ransformations is an Anglican program in
;; clojure syntax (embang.xlat). The output is a Clojure function
;; that returns either the next step as a structure incroprorating
;; continuations and parameters, or the state containing a vector of
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

(defn value-cont "returns value" [v _] v)
(defn state-cont "returns state" [_ s] s)

;; Interrupt points

(defrecord observe [id dist value cont state])
(defrecord sample [id dist cont state])
(defrecord result [state])

;; Retrieval of final result:
;; run the function on the initial state
;; and return the final state wrapped into `result'.

(defn run-cont [f s]
  (f (fn [v s] (->result (state-cont v s))) s))

(defn primitive-procedure?
  "true if the procedure is primitive,
  that is, does not have a CPS form"
  [procedure]
  (*primitive-procedures* procedure))

;; primitive procedures have to be recognized syntactically,
;; by the name. This means that a primitive procedure can
;; only appear in call position, and a primitive procedure
;; name cannot be rebound locally.

(defn simple-expression?
  "true if expr has no continuation"
  [expr]
  (if (seq? expr)
    (case (first expr)
      quote true
      fn false
      (begin
       if cond
       and or do) (every? simple-expression? (rest expr))
      let (let [[_ bindings & body] expr]
            (and (every? simple-expression?
                         (take-nth 2 (rest bindings)))
                 (every? simple-expression? body)))
      (predict
       observe
       sample
       mem
       store
       retrieve
       apply) false
      ;; application
      (and (primitive-procedure? (first expr))
           (every? simple-expression? (rest expr))))
    (not (primitive-procedure? expr))))

;; CPS transformation rules

(declare cps-of-expression)
(def ^:dynamic *gensym* 
  "customized gensym for code generation,
  bound to `symbol' in tests"
  (comp gensym (partial str "$")))

(defn ^:private cps-of-elist
  [exprs cont]
  (let [[fst & rst] exprs]
    (cps-of-expression
      fst
      (if (seq rst)
        `(~'fn [~'_ ~'$state]
           ~(cps-of-elist rst cont))
        cont))))

;; Continuation is the first, rather than the last, parameter of a
;; function to support functions with variable arguments.

(defn cps-of-fn
  "transforms function definition into CPS"
  [args cont]
  (if (vector? (first args))
    (cps-of-fn `[nil ~@args] cont)
    (let [[name parms & body] args
          fncont (*gensym* "C")]
      (binding [*primitive-procedures*
                (reduce disj *primitive-procedures* parms)]
        `(~cont (~'fn ~@(when name [name])
                  [~fncont ~'$state ~@parms]
                  ~(cps-of-elist body fncont))
                ~'$state)))))

(defn cps-of-let
  "transforms let to CPS"
  [[bindings & body] cont]
  (if (seq bindings)
    (let [[name value & bindings] bindings]
      (binding [*primitive-procedures*
                (disj *primitive-procedures* name)]
        (let [rst (cps-of-let `(~bindings ~@body) cont)]
          (if (simple-expression? value)
            `(~'let [~name ~value]
               ~rst)
            (cps-of-expression
              value
              (let [value (*gensym* "V")]
                `(~'fn [~value ~'$state]
                   (~'let [~name ~value]
                     ~rst))))))))
    (cps-of-elist body cont)))

(defmacro ^:private defn-with-named-cont
  "binds the continuation to a name to make the code
  slightly easier to reason about"
  [cps-of & args]
  (let [[docstring [parms & body]]
        (if (string? (first args)) 
          [(first args) (rest args)]
          [(format "CPS transformation macro '%s'" cps-of) args])]
    (let [cont (last parms)]
      `(defn ~(with-meta cps-of {:doc docstring})
         ~parms
         (if (symbol? ~cont)
           (do ~@body)
           (let [~'named-cont (*gensym* "C")]
             `(~~''let [~~'named-cont ~~cont]
                ~(~cps-of ~@(butlast parms) ~'named-cont))))))))

(defn-with-named-cont
  cps-of-if
  "transforms if to CPS"
  [args cont]
  (let [[cnd thn els & rst] args]
    (assert (empty? rst)
            (format "Invalid number of args (%d) passed to if"
                    (count args)))
    (if (simple-expression? cnd)
      `(~'if ~cnd
         ~(cps-of-expression thn cont)
         ~(cps-of-expression els cont))
      (cps-of-expression
        cnd
        (let [cnd (*gensym* "I")]
          `(~'fn [~cnd ~'$state]
             (~'if ~cnd
               ~(cps-of-expression thn cont)
               ~(cps-of-expression els cont))))))))

(defn-with-named-cont
  cps-of-cond
  "transforms cond to CPS"
  [clauses cont]
  (if (seq clauses)
    (let [[cnd thn & clauses] clauses]
      (cps-of-if [cnd thn `(~'cond ~@clauses)] cont))
    (cps-of-expression nil cont)))

(defn-with-named-cont
  cps-of-and
  "transforms and to CPS"
  [args cont]
  (if (seq args)
    (let [[cnd & args] args]
      (if (seq args)
        (cps-of-expression
          cnd
          (let [cnd (*gensym* "I")]
            `(~'fn [~cnd ~'$state]
               ~(cps-of-if [cnd `(~'and ~@args) cnd] cont))))
        (cps-of-expression cnd cont)))
    (cps-of-expression true cont)))

(defn-with-named-cont
  cps-of-or
  "transforms or to CPS"
  [args cont]
  (if (seq args)
    (let [[cnd & args] args]
      (if (seq args)
        (cps-of-expression
          cnd
          (let [cnd (*gensym* "I")]
            `(~'fn [~cnd ~'$state]
               ~(cps-of-if [cnd cnd `(~'or ~@args)] cont))))
        (cps-of-expression cnd cont)))
    (cps-of-expression false cont)))

(defn cps-of-do
  "transforms do to CPS"
  [exprs cont]
  (cps-of-elist exprs cont))

(defn ^:private make-of-args
  "builds lexical bindings for all compound args
  and then calls `make' to build expression
  out of the args; used by predict, observe, sample, application"
  ([args make] (make-of-args args false make))
  ([args first-is-rator make]
     (let [substs (map (fn [arg is-rator]
                         (if (or (simple-expression? arg)
                                 (and is-rator
                                      (primitive-procedure? arg)))
                           [nil arg]
                           [arg (*gensym* "A")]))
                       args
                       (cons first-is-rator (repeat false)))]
       (letfn [(make-of-slist [slist]
                 (if (seq slist)
                   (let [[[arg subst] & slist] slist]
                     (if arg ;; is a compound expression
                       (cps-of-expression
                         arg
                         `(~'fn [~subst ~'$state]
                            ~(make-of-slist slist)))
                       (make-of-slist slist)))
                   (make (map second substs))))]

         (make-of-slist substs)))))

(defn cps-of-predict
  "transforms predict to CPS,
  predict appends predicted expression
  and its value to (:predicts $state)"
  [args cont]
  (make-of-args args
                (fn [[label value]]
                  `(~cont nil (add-predict ~'$state
                                           ~label ~value)))))

(defn cps-of-observe
  "transforms observe to CPS,
  observe updates the weight by adding
  the result of observe (log-probability)
  to the log-weight"
  [args cont]
  (make-of-args args
                (fn [[dist value]]
                  `(->observe '~(*gensym* "O")
                              ~dist ~value ~cont ~'$state))))

(defn cps-of-sample
  "transforms sample to CPS;
  on sample the program is interrupted
  and the control is transferred to the inference algorithm"
  [args cont]
  (make-of-args args
                (fn [[dist]]
                  `(->sample '~(*gensym* "S")
                             ~dist ~cont ~'$state))))

(defn cps-of-mem
  "transforms mem to CPS"
  [[[_ & args]] cont]
  (let [mcont (*gensym* "C")
        id (*gensym* "M")
        value (*gensym* "V")
        mparms (*gensym* "P")
        [name parms & body] (if (vector? (first args))
                              `(nil ~@args)
                              args)]
    `(~cont (~'fn ~@(when name [name])
              [~mcont ~'$state & ~mparms]
              (~'if (in-mem? ~'$state '~id ~mparms)
                ;; continue with stored value
                (~mcont (get-mem ~'$state '~id ~mparms) ~'$state)
                ;; apply the function to the arguments with
                ;; continuation that intercepts the value
                ;; and updates the state
                ~(cps-of-expression
                  `(~'apply (~'fn ~parms ~@body) ~mparms)
                  `(~'fn [~value ~'$state]
                     (~mcont ~value
                             (set-mem ~'$state
                                      '~id ~mparms ~value))))))
            ~'$state)))

(defn cps-of-store
  "transforms store to CPS;
  the continuation receives the stored value"
  [args cont]
  (make-of-args args
                (fn [args]
                  `(~cont ~(last args)
                          (store ~'$state ~@args)))))

(defn cps-of-retrieve
  "transforms retrieve to CPS"
  [args cont]
  (make-of-args args
                (fn [args]
                  `(~cont (retrieve ~'$state ~@args)
                          ~'$state))))

(defn cps-of-apply
  "transforms apply to CPS;
  apply of user-defined (not primitive) procedures
  is trampolined --- wrapped into a parameterless closure"
  [args cont]
  (make-of-args args :first-is-rator
                (fn [acall]
                  (let [[rator & rands] acall]
                    (if (primitive-procedure? rator)
                      `(~cont (apply ~@acall) ~'$state) ; clojure `apply'
                      `(~'fn [] (apply ~rator
                                       ~cont ~'$state ~@rands)))))))

(defn cps-of-application
  "transforms application to CPS;
  application of user-defined (not primitive) procedures
  is trampolined --- wrapped into a parameterless closure"
  [exprs cont]
  (make-of-args exprs :first-is-rator
                (fn [call]
                  (let [[rator & rands] call]
                    (if (primitive-procedure? rator)
                      `(~cont ~call ~'$state)
                      `(~'fn [] (~rator ~cont ~'$state ~@rands)))))))

(defn cps-of-primitive-procedure
  "wraps primitive procedure"
  [expr cont]
  (let [fncont (*gensym* "C")
        parms (*gensym* "P")]
    `(~cont (~'fn [~fncont ~'$state & ~parms]
              (~fncont (~'apply ~expr ~parms) ~'$state))
            ~'$state)))

(defn cps-of-expression
  "dispatches CPS transformation by expression type"
  [expr cont]
  (cond
    (simple-expression?
      expr) `(~cont ~expr ~'$state)
    (seq?
      expr) (let [[kwd & args] expr]
              (case kwd
                fn        (cps-of-fn args cont)
                let       (cps-of-let args cont)
                if        (cps-of-if args cont)
                cond      (cps-of-cond args cont)
                and       (cps-of-and args cont)
                or        (cps-of-or args cont)
                do        (cps-of-do args cont)
                predict   (cps-of-predict args cont)
                observe   (cps-of-observe args cont)
                sample    (cps-of-sample args cont)
                mem       (cps-of-mem args cont)
                store (cps-of-store args cont)
                retrieve (cps-of-retrieve args cont)
                apply     (cps-of-apply args cont)
                ;; application
                (cps-of-application expr cont)))
    (primitive-procedure?
      expr) (cps-of-primitive-procedure expr cont)
    :else (assert false)))

(def ^:dynamic *primitive-procedures*
  "primitive procedures, do not exist in CPS form"
  (let [;; exclude procedures for which CPS counterparts are provided
        exclude '#{map reduce}
        ;; collect all public functions in clojure.core
        core (keep (fn [[k v]]
                     (when (and (not (exclude k))
                                (fn? (var-get v)))
                       k))
                   (ns-publics 'clojure.core))]
    ;; add functions provided by the runtime
    (into (set core)
          '[;; custom math tests
            isfinite? isnan?

            ;; scalar arithmetics
            inc dec
            + - * / mod
            abs floor ceil round
            sin cos tan asin acos atan
            sinh cosh tanh
            log log10 exp
            pow cbrt sqrt

            ;; sequence operations
            sum mean normalize range

            ;; ERPs (alphabetically)
            beta
            binomial
            dirichlet
            discrete
            exponential
            flip
            gamma
            normal
            poisson
            uniform-continuous
            uniform-discrete])))
