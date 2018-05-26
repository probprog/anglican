(ns anglican.emit
  "Top-level forms for Anglican programs"
  (:require [anglican.xlat :refer [program alambda]]
            [anglican.trap :refer [*gensym* *checkpoint-gensym*
                                   shading-primitive-procedures
                                   cps-of-expression result-cont 
                                   fn-cps mem-cps primitive-procedure-cps]]
            [anglican.runtime :refer [distribution]]
            [anglican.inference :refer [infer equalize]]
            [anglican.state :refer [get-log-weight get-predicts get-result]])
  #?(:cljs (:require-macros [anglican.emit :refer [query query* defquery
                                                  fm fm* defm
                                                  with-primitive-procedures]])))


;; TODO move or document purpose better
(defn- cljs-env?
  "Take the &env from a macro, and tell whether we are expanding into cljs."
  [env]
  (boolean (:ns env)))

#?(:clj
   (defmacro if-cljs
     "Return then if we are generating cljs code and else for Clojure code.
     https://groups.google.com/d/msg/clojurescript/iBY5HaQda4A/w1lAQi9_AwsJ"
     [then else]
     (if (cljs-env? &env) then else)))



;; TODO move
#?(:cljs
   (defn format
     "Similar to Java String's format function for cljs."
     [s & args]
     (goog.string.format s (into-array args))))



;;;; Top-level forms for Anglican programs

;;; Helper: stable gensyms

;; Anglican uses anglican.trap/*gensym* to generate
;; automatic checkpoint identifiers. This works well
;; for inference in general, however there are use cases
;; where predictable identifiers which stay the same
;; between compilations suite better: debugging, compiled
;; inference are some examples.

(defn ^:private make-stable-gensym 
  "Creates a function which returns a fresh identifier
  of form name-prefix#, where # is a counter starting at 1."
  [name]
  (let [i (atom 0)]              
    (fn [& [prefix]]
      (swap! i inc)
      (symbol (str name "-" (or prefix "G") @i)))))

;;; Code manipulation

;; Higher-order functions cannot be re-used from Clojure,
;; they have to be re-implemented in CPS. Clojure has
;; many higher-order functions in the clojure.core
;; namespace, some of which are rather esoteric and not
;; quite appropriate for a small language like Anglican.
;; Still, some higher-order functions are used ubiquitously 
;; in functional programs, and should be available by
;; default.

(defn ^:private overriding-higher-order-functions
  "binds names of essential higher-order functions
  to their CPS implementations"
  [& body]
  `(~'let [~@(mapcat (fn [fun]
                       [fun (symbol "anglican.emit" (str "$" fun))]) 
                     '[map reduce
                       filter some
                       repeatedly
                       comp partial
                       conditional])]
     ~@body))

;; The main program (or query) is defined using the `query'
;; macro and can also be immediately bound to a symbol
;; using the `defquery' macro.

(defmacro query*
  "Helper macro called by query and defquery. The name is mandatory."
  [name & args]
  (let [[value source]
        (if (or (symbol? (first args)) (vector? (first args)))
          [(first args) (rest args)]
          ['$value args])]
    (overriding-higher-order-functions
      (shading-primitive-procedures (if (vector? value) value [value])
        (binding [*checkpoint-gensym* (make-stable-gensym name)]
          `(with-meta
             (~'fn ~name [~value ~'$state]
               ~(cps-of-expression `(~'do ~@source)
                                   `(if-cljs
                                     anglican.trap/result-cont
                                     result-cont)))
             {:source '(~'query ~name ~@args)}))))))

(defmacro query
  "Defines an anglican query. Syntax:

     (query optional-name optional-parameter 
       anglican-expression ...)

  Example:

     (query
       (let [x (sample (normal 0 10))]
          (observe (normal 1 1) x)
          (predict x)))

  `query' returns a value that can be passed as the second
  argument to `anglican.infer/infer'. An optional parameter can
  be specified and must either a symbol or a vector, in which
  case the initial value passed to the query is destructured to
  the parameter:

     (query [mean sd]
        (let [x (sample (normal mean sd))]
          (predict x)))"
  [& args]
  (if (and (seq (nthnext args 2)) ;; for backward compatibility
           (symbol? (first args)) 
           (or (symbol? (second args)) (vector? (second args))))
    `(query* ~@args)
    `(query* ~(*gensym* "query") ~@args)))

(defmacro defquery
  "Binds variable to an anglican query. Syntax:

      (defquery variable-name optional-doc-string optional-parameter
        anglican-expression ...)

  A defquery declaration is a shortcut for

      (def variable-name optional-doc-string
        (query optional-parameter
          anglican-expression ...)"
  [name & args]
  (let [[docstring source]
        (if (string? (first args))
          [(first args) (rest args)]
          [(format "anglican program '%s'" name) args])]
    `(def ~(with-meta name {:doc docstring})
       (query* ~name ~@source))))

;; A query can be seen as a random source, that is a distribution.
;; Conditional wraps a query into a parameterised distribution
;; constructor.

(defn conditional
  "accepts an Anglican query and returns
  a conditional distribution defined by the query"
  [query & options]

  ;; Algorithm parameters are an optional sequence of the form
  ;;   [inference-algorithm & algorithm-options]
  ;; with importance sampling as the default inference algorithm.
  (let [[algorithm & options] options
        algorithm (or algorithm :importance)]

    ;; Conditional distribution is a function which, when applied
    ;; to the values (argument of query), returns a distribution
    ;; object.
    (fn [& value]

      ;; Since sampling from a distribution object is
      ;; unweighted, the random source for the distribution is
      ;; built around an equalized sequence of inferred samples.
      ;; Sampling is imlemented by removing the first element
      ;; from a lazy sequence of equalized samples.

      (let [;; Random source is a mutable reference.
            source (-> (apply infer algorithm
                              query value options)
                       equalize
                       #?(:clj ref
                         :cljs atom))
            ;; Next sample is the first sample removed
            ;; from the lazy sequence in the source.
            next-sample #?(:clj
                          #(dosync
                           (let [[sample & samples] @source]
                             (ref-set source samples)
                             sample))
                          :cljs
                          #(let [[sample & samples] @source]
                            (reset! source samples)
                            sample))]
      (reify distribution

        ;; A sample from the distribution is the collection
        ;; of predicts in a single sample from the inferred
        ;; sample sequence.
        (sample* [this]
          (get-result (next-sample)))

        ;; Observing a value requires source code analysis,
        ;; not implemented yet. For a future implementation,
        ;; the source code of query is in the meta-data for
        ;; key `:source'.
        (observe* [this value]
          (throw (ex-info "not implemented"
                          {:value value}))))))))

;; The original Scheme-like syntax is now deprecated, but
;; supported for compatibility with older programs.  Programs
;; written in the legacy syntax are defined using `anglican' and
;; bound to a name using `defanglican'.

(defmacro ^:deprecated anglican 
  "macro for embedding anglican programs"
  [& args]
  (let [[value source]
        (if (symbol? (first args)) ; named argument?
          [(first args) (rest args)]
          ['$value args])]
    (overriding-higher-order-functions
      (shading-primitive-procedures [value]
        `(with-meta
           (~'fn ~(*gensym* "anglican") [~value ~'$state]
             ~(cps-of-expression (program source) result-cont))
           {:source '(~'anglican ~@args)})))))

(defmacro ^:deprecated defanglican
  "binds variable to anglican program"
  [name & args]
  (let [[docstring source]
        (if (string? (first args))
          [(first args) (rest args)]
          [(format "anglican program '%s'" name) args])]
    `(def ~(with-meta name {:doc docstring})
       (anglican ~@source))))

;;; Auxiliary macros

;; When a function is defined outside an Anglican program, it
;; must be in CPS form. fm and defm are like fn and defn but
;; automatically transform functions into CPS.

(defmacro fm*
  "Helper macro called by fm and defm. The name is mandatory."
  [name & args]
  (overriding-higher-order-functions
    (binding [*checkpoint-gensym* (make-stable-gensym name)]
      (fn-cps args))))

(defmacro fm
  "Defines an anglican function outside of anglican code.
  The syntax is the same as of `fn' with a single parameter list:

     (fm optional-name [parameter ...] 
        anglican-expression ...)"
  [& args]
  (if (vector? (first args))
    `(fm* ~(*gensym* "fn") ~@args)
    `(fm* ~@args)))

(defmacro defm
  "Binds a variable to an anglican function. The syntax is the same as for
  `defn' with a single parameter list:

      (defm variable-name optional-doc-string [parameter ...]
         anglican-expression ...)

  A `defm' declaration is a shortcut for:

      (def variable-name optional-doc-string
         (fm variable-name [parameter ...]
             anglican-expression ...))"
  [name & args]
  (let [[docstring source]
        (if (string? (first args))
          [(first args) (rest args)]
          [(format "CPS function '%s'" name) args])
        arglist (first source)]
    `(def ~(with-meta name
             {:doc docstring
              :arglists `'([~'$cont ~'$state
                            ~@(first source)])})
       (fm* ~name ~@source))))

;; In anglican, memoized computations, which can be impure, are
;; created by `mem'. Inside a CPS expression, `mem' is
;; intrepreted as a special form. In a Clojure context, the macro
;; `mem' replicates the functionality of `mem' in anglican.

(defmacro mem
  "creates a memoized computation in CPS form"
  [& args]
  (overriding-higher-order-functions
    (mem-cps args)))

;; The legacy names for fm and defm are cps-fn and def-cps-fn.
;; These names are deprecated but preserved for compatibility.

(defmacro ^:deprecated cps-fn
  "legacy name for fm"
  [& args]
  `(fm ~@args))

(defmacro ^:deprecated def-cps-fn
  "legacy name for defm"
  [& args]
  `(defm ~@args))

;; Functions can also be defined in Scheme-like rather
;; than Clojure syntax. Again, this syntax is deprecated
;; but preserved for compatibility.

(defmacro ^:deprecated lambda
  "defines function in Anglican syntax"
  [& args]
  `(fm ~@(next (alambda nil args))))

(defmacro ^:deprecated defun
  "binds variable to function in Anglican syntax"
  [name & args]
  `(defm ~@(next (alambda name args))))

;; Any non-CPS procedures can be used in the code,
;; but must be wrapped and re-bound.

(defmacro
  with-primitive-procedures
  "binds primitive procedure names to their CPS versions;
  if procedure name is qualified, it becomes unqualified in
  the scope of the macro"
  [procedures & body]
  `(let [~@(mapcat (fn [proc] 
                     [(symbol (name proc))
                      (primitive-procedure-cps proc)])
                   procedures)]
     ~@body))

;;;; Program prologue

;; Common anglican code that must be available to all
;; programs goes here. 

;;; CPS versions of higher-order functions

;; Higher-order functions must be re-implemented in CPS,
;; such that both the functions themselves and the
;; functional arguments follow the CPS calling convention.
;; Essential higher-order functions are implemented here and
;; rebound in macros defining anglican code by calling
;; `overriding-higher-order-functions'.

(declare $map $reduce
         $filter $some
         $repeatedly
         $comp $partial
         $conditional)

(defm ^:private $map1 
  "map on a single sequence"
  [fun lst]
  (if (empty? lst) nil
      (cons (fun (first lst))
            ($map1 fun (rest lst)))))

(defm ^:private $nils? 
  "true if the list contains nil"
  [lst]
  (and (seq lst)
       (or (nil? (first lst))
           ($nils? (rest lst)))))

(defm $map 
  "map in CPS"
  [fun & lsts]
  (if ($nils? ($map1 seq lsts)) nil
    (cons (apply fun ($map1 first lsts))
          (apply $map fun ($map1 rest lsts)))))

(defm ^:private $reduce1
  "reduce with explicit init in CPS"
  [fun init lst]
  (if (empty? lst) init
      ($reduce1 fun
                (fun init (first lst))
                (rest lst))))

(defm $reduce
  "reduce in CPS"
  [fun & args]
  (let [init (if (seq (rest args))
               (first args) 
               (first (first args)))
        lst (if (seq (rest args))
              (second args)
              (rest (first args)))]
    ($reduce1 fun init lst)))

(defm $filter
  "filter in CPS"
  [fun lst]
  (cond
   (empty? lst) lst
   (fun (first lst)) (cons (first lst) ($filter fun (rest lst)))
   :else ($filter fun (rest lst))))

(defm $some
  "some in CPS"
  [fun lst]
  (and (seq lst)
    (or (fun (first lst))
        ($some fun (rest lst)))))

;; `repeatedly' is useful for sampling from multivariates
;; using `transform-sample'
(defm $repeatedly
  "repeatedly in CPS"
  [n thunk]
  (if (zero? n) nil
    (cons (thunk) ($repeatedly (- n 1) thunk))))

(defm $comp
  "comp in CPS"
  [& funs]
  (if funs
    (let [funs (reverse funs)]
      (fn [& args]
        ($reduce (fn [res fun] (fun res))
                 (apply (first funs) args) (rest funs))))
    identity))

(defm $partial
  "partial in CPS"
  [fun & bound]
  (fn [& free] (apply fun (concat bound free))))

(defn $conditional
  "conditional returning CPS-transformed function"
  [cont $state & condargs]
  (cont (fn [cont $state & distargs]
          (let [constructor (apply conditional condargs)]
            (cont (apply constructor distargs) $state)))
        $state))
