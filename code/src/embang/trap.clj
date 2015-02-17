(ns embang.trap
  (:require embang.runtime)
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

;; Retrieval of final result.

(defn result-cont [v s] (->result s))

(defn primitive-procedure?
  "true if the procedure is primitive,
  that is, does not have a CPS form"
  [procedure]
  (*primitive-procedures* procedure))

(defn fn-form?
  "true when the argument is a fn form"
  [expr]
  (and (seq? expr) (= (first expr) 'fn)))

;;; Simple expressions

;; Simple expressions are passed to continuations
;; unmodified.

(defn simple?
  "true if expr has no continuation"
  [expr]
  (if (and (seq? expr) (seq expr))
    (case (first expr)
      quote true
      fn false
      (begin
       if cond 
       and or do) (every? simple? (rest expr))
      case (if (even? (count expr))
             ;; No default clause.
             (every? simple? (take-nth 2 (rest expr)))
             ;; Default clause is a single expression.
             (and (every? simple? (take-nth 2 (rest expr)))
                  (simple? (last expr))))
      let (let [[_ bindings & body] expr]
            (and (every? simple?
                         (take-nth 2 (rest bindings)))
                 (every? simple? body)))
      (predict
       observe
       sample
       mem
       store
       retrieve
       apply) false
      ;; application
      (and (primitive-procedure? (first expr))
           (every? simple? (rest expr))))
    (not (primitive-procedure? expr))))

;;; Opaque expresions

;; Some expressions are opaque, they are transformed
;; into CPS and then passed to continuations as arguments.

(defn opaque?
  "true when the argument is an expression
  which is passed to continuation (rather
  than accepts continuation) in its CPS form"
  [expr]
  (or (simple? expr)
      (primitive-procedure? expr)
      (fn-form? expr)))
 
;; Simple expressions, primitive procedure wrappers
;; and fn forms are opaque.

(declare primitive-procedure-cps fn-cps)
(defn opaque-cps
  "return CPS form of an opaque expression"
  [expr] {:pre [(opaque? expr)]}
  (cond
   (simple? expr) expr
   (primitive-procedure? expr) (primitive-procedure-cps expr)
   (fn-form? expr) (fn-cps (rest expr))))

;;; General CPS transformation rules

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

(defn fn-cps
  "transforms function definition to CPS form"
  [args]
  (if (vector? (first args))
    (fn-cps `[nil ~@args])
    (let [[name parms & body] args
          fncont (*gensym* "C")]
      (binding [*primitive-procedures*
                (reduce disj *primitive-procedures* parms)]
        `(~'fn ~@(when name [name])
           [~fncont ~'$state ~@parms]
           ~(cps-of-elist body fncont))))))

(defn cps-of-let
  "transforms let to CPS"
  [[bindings & body] cont]
  (if (seq bindings)
    (let [[name value & bindings] bindings]
      (binding [*primitive-procedures*
                (disj *primitive-procedures* name)]
        (let [rst (cps-of-let `(~bindings ~@body) cont)]
          (if (opaque? value)
            `(~'let [~name ~(opaque-cps value)]
               ~rst)
            (cps-of-expression
              value
                `(~'fn [~name ~'$state]
                   ~rst))))))
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
    (if (opaque? cnd)
      `(~'if ~(opaque-cps cnd)
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
  cps-of-case
  "transforms case to CPS"
  [args cont]
  (let [[key & clauses] args]
    (if (opaque? key)
      `(~'case ~(opaque-cps key)
         ~@(mapcat (fn [[tag expr :as clause]]
                     (if (= (count clause) 2)
                       [tag (cps-of-expression expr cont)]
                       [(cps-of-expression expr cont)]))
                   (partition 2 clauses)))
      (cps-of-expression
        key
        (let [key (*gensym* "K")]
          `(~'fn [~key ~'$state]
             ~(cps-of-expression
                `(~'case ~key ~@clauses) cont)))))))

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
                         (cond
                          (and is-rator
                               (primitive-procedure? arg)) [nil arg]
                          (opaque? arg) [nil (opaque-cps arg)]
                          :else [arg (*gensym* "A")]))
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
  [[arg & _ :as args] cont] {:pre [(= (count args) 1)]}
  (let [mcont (*gensym* "C")
        id (*gensym* "M")
        value (*gensym* "V")
        mparms (*gensym* "P")

        ;; If the argument of mem is a named lambda,
        ;; move the name to outer function.
        [name expr] (if (and (seq? arg)
                             (= (first arg) 'fn)
                             (symbol? (second arg)))
                      [(second arg) `(~'fn ~@(nnext arg))]
                      [nil arg])]

    `(~cont (~'let [~id (~'gensym "M")]
              (~'fn ~@(when name [name])
                [~mcont ~'$state & ~mparms]
                (~'if (in-mem? ~'$state ~id ~mparms)
                  ;; continue with stored value
                  (~mcont (get-mem ~'$state ~id ~mparms) ~'$state)
                  ;; apply the function to the arguments with
                  ;; continuation that intercepts the value
                  ;; and updates the state
                  ~(cps-of-expression
                     `(~'apply ~expr ~mparms)
                     `(~'fn [~value ~'$state]
                        (~mcont ~value
                                (set-mem ~'$state
                                         ~id ~mparms ~value)))))))
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

(defn primitive-procedure-cps
  "wraps primitive procedure as a CPS form"
  [expr]
  (let [fncont (*gensym* "C")
        parms (*gensym* "P")]
    `(~'fn [~fncont ~'$state & ~parms]
       (~fncont (~'apply ~expr ~parms) ~'$state))))
    
(defn cps-of-expression
  "dispatches CPS transformation by expression type"
  [expr cont]
  (cond
    (opaque?
      expr) `(~cont ~(opaque-cps expr) ~'$state)
    (seq?
      expr) (let [[kwd & args] expr]
              (case kwd
                let       (cps-of-let args cont)
                if        (cps-of-if args cont)
                cond      (cps-of-cond args cont)
                case      (cps-of-case args cont)
                and       (cps-of-and args cont)
                or        (cps-of-or args cont)
                do        (cps-of-do args cont)
                predict   (cps-of-predict args cont)
                observe   (cps-of-observe args cont)
                sample    (cps-of-sample args cont)
                mem       (cps-of-mem args cont)
                store     (cps-of-store args cont)
                retrieve  (cps-of-retrieve args cont)
                apply     (cps-of-apply args cont)
                ;; application
                (cps-of-application expr cont)))
     :else (assert false)))

(def ^:dynamic *primitive-procedures*
  "primitive procedures, do not exist in CPS form"
  (let [;; higher-order procedures cannot be primitive
        exclude '#{loop
                   map reduce
                   filter keep keep-indexed remove
                   repeatedly
                   every? not-any? some
                   every-pred some-fn
                   comp juxt partial}
        ;; runtime namespaces
        runtime-namespaces '[clojure.core embang.runtime]]
    (set (keep (fn [[k v]]
                 (when (and (not (exclude k))
                            (fn? (var-get v)))
                   k))
               (mapcat ns-publics runtime-namespaces)))))
