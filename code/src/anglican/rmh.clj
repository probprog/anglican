(ns anglican.rmh
  "Random-walk Metropolis-Hastings"
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:use [anglican.state :exclude [initial-state]]
        anglican.inference
        [anglican.runtime :only [observe sample]]))

;;;; Random-walk Metropolis-Hastings

(derive ::algorithm :anglican.inference/algorithm)

;;; Initial state

(def initial-state
  "initial state for RMH"
  (into anglican.state/initial-state
        ;; The state is extended by the trace ---
        ;; the vector of current random choices,
        ;; and the random database --- random choices
        ;; from the previous particle.
        {::trace []                   ; current random choices
         ::rdb {}                     ; stored random choices {choice-id value ...}
         ::choice-counts {}           ; counts of occurences of each `sample'
         ::choice-last-id nil         ; last sample id
         ::chosen-entry-id nil        ; id of the entry chosen for resampling from a kernel distribution k(x'_{nk} | x_{nk}) (or the other way)
         ::chosen-entry-value nil     ; value of the entry chosen for resampling from the kernel
         ::log-new-val-from-prior nil ; log p(x'_{nk} | previous choices of the execution trace)
         ::log-kappa nil              ; log k(this state's chosen entry | prev state's chosen entry)
         ::alpha 0.5                  ; probability of sampling from alternative proposal (from 0 to 1)
         ::sigma 1}))                 ; std of alternative proposal.

;; Alternative proposals
;;   only supports
;;   exponential ---> folded-normal
;;   gamma       ---> folded-normal-positive
;;   normal      ---> normal
;;   poisson     ---> folded-normal-discrete
;;   beta        ---> uniform-continuous
(defn get-alt-proposal
  "given the value and distribution of x_{nk}, returns an alternative proposal
  distribution (runtime/distribution protocol) which is centered around x_{nk}
  with some closeness factor sigma"
  [distribution value sigma]
  (condp = (type distribution)
    anglican.runtime.exponential-distribution (anglican.runtime/folded-normal value sigma)
    anglican.runtime.gamma-distribution (anglican.runtime/folded-normal-positive value sigma)
    anglican.runtime.normal-distribution (anglican.runtime/normal value sigma)
    anglican.runtime.poisson-distribution (anglican.runtime/folded-normal-discrete value sigma)
    anglican.runtime.beta-distribution (anglican.runtime/uniform-continuous 0 1)
    distribution))

;; Kernel proposals
(defn get-kernel-proposal
  "returns a kernel proposal distribution (runtime/distribution protocol)
  k( . | x_{nk}) given the the value and the distribution of x_{nk}. The kernel
  is constructed by choosing an alternative proposal distribution using a
  biased coin flip with the success probability alpha."
  [distribution value alpha sigma]
  (let [alt-proposal (get-alt-proposal distribution value sigma)]
    (reify anglican.runtime/distribution
      (sample [this]
              (if (sample (anglican.runtime/flip alpha))
                (sample alt-proposal)
                (sample distribution)))
      (observe [this value]
               (Math/log (+ (* alpha
                               (Math/exp (observe alt-proposal value)))
                            (* (- 1 alpha)
                               (Math/exp (observe distribution value)))))))))

;;; Trace

;; ALMH need access to the trace, expose it via an accessor function.
(defn get-trace "returns trace" [state] (state ::trace))

;; The trace is a vector of entries
;;   {choice-id value log-p cont}
;; where
;;   - `choice-id' is the identifier of the random choice,
;;   - `value' is the value of random choice in the current run,
;;   - `log-p' is the log probability (mass or density) of
;;     the value given the distribution,
;;   - `cont' is the continuation that starts at the checkpoint.

(defrecord entry [choice-id value log-p cont])

(defn choice-id
  "returns a unique idenditifer for sample checkpoint
  and the updated state"
  [smp state]
  (checkpoint-id smp state ::choice-counts ::choice-last-id))

(defn record-choice
  "records random choice in the state"
  [state choice-id value log-p cont]
  (update-in state [::trace]
             conj (->entry choice-id value log-p cont)))

;;; Random database (RDB)

;; RDB is a mapping from choice-ids to the chosen values.

(defn rdb
  "creates random database from trace"
  [trace]
  (into {} (map (fn [entry]
                  [(:choice-id entry) (:value entry)])
                trace)))

;;; Inference

(defmethod checkpoint [::algorithm anglican.trap.sample] [_ smp]
  (let [[choice-id state] (choice-id smp (:state smp))

        current-value (state ::chosen-entry-value)
        use-kernel (= choice-id (::chosen-entry-id state))

        kappa (if use-kernel
                (get-kernel-proposal (:dist smp) current-value (::alpha state) (::sigma state)))
        value (if use-kernel
                ;; Sample from k(next | prev)
                (sample kappa)
                ;; Continue without sampling from k(next | prev)
                (if (contains? (state ::rdb) choice-id)
                  ;; Reuse value from RDB
                  ((state ::rdb) choice-id)
                  ;; Sample new value
                  (sample (:dist smp))))

        ;; Verifying whether reused sample is in support
        log-p (if (not use-kernel)
                (try (observe (:dist smp) value)
                  ;; NaN is returned if value is not the same type as the
                  ;; distribution e.g. (observe (runtime/normal 0 1) false)
                  (catch Exception e (/ 0. 0.))))
        value (if (not use-kernel)
                ;; Outside of the support when
                ;;   - probability mass is zero, i.e. log-p is -Infinity
                ;;   - wrong argument, e.g.
                ;;     (observe (runtime/gamma 0 -1) 1) returns error
                ;;   - wrong value type, e.g.
                ;;     (observe (runtime/normal 0 1) true) returns error
                ;;   - NaN
                (if (< (/ -1. 0.) log-p (/ 1. 0.))
                  value
                  ;; The retained value is not in support,
                  ;; resample the value from the prior.
                  (sample (:dist smp)))
                value)

        ;; Score sample
        log-p (if use-kernel
                (observe kappa value)
                (observe (:dist smp) value))

        cont (fn [_ update]
               ;; Continuation which starts from this checkpoint
               ;; --- called when the random choice is selected
               ;; for resampling.
               (update-in smp [:state]
                          ;; Update fields override state fields.
                          (fn [state]
                            (merge-with #(or %2 %1) state update))))
        state (record-choice state choice-id value log-p cont)

        ;; Store log-kappa probability k(x'_{nk} | x_{nk})
        state (if use-kernel
                (assoc state ::log-kappa log-p)
                state)

        ;; Store probability of x'_{nk} under prior,
        ;; p(x'_{nk} | previous random choices)
        log-p-from-prior (if use-kernel (observe (:dist smp) value))
        state (if use-kernel
                (assoc state ::log-new-val-from-prior log-p-from-prior)
                state)]
    #((:cont smp) value state)))

;;; State transition

;; Optional `update' argument is used by Adaptive LMH to supply
;; additional fields. The state is extensible, so are state
;; transformation methods.

(defn next-state
  "produces next state given current state
  and the trace entry to resample"
  ([state entry] (next-state state entry {}))
  ([state entry update]
   (:state (exec ::algorithm (:cont entry) nil
                 ;; Pass on the RDB \ {x_{nk}} + an extra fields ::chosen-entry
                 ;; and ::chosen-entry-value
                 ;; This is because we don't want x_{nk} to be included in
                 ;; get-log-retained-probability
                 (into update
                       {::rdb (dissoc
                               (rdb (state ::trace))
                               (:choice-id entry))
                        ::chosen-entry-id (:choice-id entry)
                        ::chosen-entry-value (:value entry)})))))

(defn prev-state
  "produces previous state given the current and
  the next state and the resampled entry
  by re-attaching new rdb to the original state"
  ([state next-state entry] (prev-state state next-state entry {}))
  ([state next-state entry update]
   (let [current-sample (trampoline (:cont entry) nil state)
         old-value (:value entry)

         ;; Value in the trace of the new-state that has the same choice-id as entry
         new-value (first
                    (keep #(if (= (:choice-id %) (:choice-id entry))
                             (:value %))
                          (next-state ::trace)))

         ;; TEST printout
         ;; _ (prn "prev-state: new-value" new-value)

         kappa-reverse (get-kernel-proposal (:dist current-sample)
                                            new-value
                                            (::alpha state)
                                            (::sigma state))]
     (merge-with #(or %2 %1) state ; replaces state's rdb with next-state's rdb
                 (into update
                       {;; Use trace of next-state as RDB; Remove the x'_{nk}
                        ;; so that it doesn't get included in
                        ;; get-log-retained-probability
                        ::rdb (dissoc
                                (rdb (next-state ::trace))
                                (:choice-id entry))

                        ;; Use trace of old-state as trace
                        ::trace (state ::trace)

                        ;; Store log-kappa probability k(x_{nk} | x'_{nk})
                        ::log-kappa (observe kappa-reverse old-value)

                        ;; Store log probability of the old choice under prior
                        ;; p(x_{nk} | previous random choices)
                        ::log-new-val-from-prior (observe (:dist current-sample) old-value)})))))

;; Transition probability

(defn get-log-retained-probability
  "computes log probability of retained random choices"
  [state]
  (reduce + (keep
              (fn [{:keys [choice-id value log-p]}]
                (when (and (contains? (state ::rdb) choice-id)
                           (= value ((state ::rdb) choice-id)))
                  log-p))
              (state ::trace))))

(defn get-log-kappa
  "computes log probability arising from the transition kappa: k(x'_{nk} | x_{nk})"
  [state]
  (state ::log-kappa))

(defn get-log-new-val-from-prior
  "computes log probability of the new choice x'_{nk} pretending it was
  sampled under prior"
  [state]
  (state ::log-new-val-from-prior))

(defn utility
  "computes state utility, used to determine
  the acceptance log-probability as (next-utility - prev-utility)"
  [state]
  (+ (get-log-weight state)
     (get-log-retained-probability state)
     (get-log-new-val-from-prior state)
     (- (get-log-kappa state))
     (- (Math/log (count (state ::trace))))))

(defn correct-log-weight
  "corrects log weight of a sample, setting it to 0
  if the sample is in the support, -Infinity otherwise"
  [state]
  (let [log-weight (get-log-weight state)
        corrected-log-weight (if (> log-weight (/ -1. 0.))
                               0. (/ -1. 0.))]
    (set-log-weight state corrected-log-weight)))

(defmethod infer :rmh [_ prog value & {:keys [alpha sigma] :or {alpha 0.5 sigma 1}}]
  (assert (and (>= alpha 0) (<= alpha 1))
          ":alpha must be in [0 1]")
  (assert (> sigma 0)
          ":sigma must be positive")
  (letfn
    [(sample-seq [state]
       (lazy-seq
         (let [;; Choose uniformly a random choice to resample.
               entry (rand-nth (state ::trace))
               ;; Compute next state from the resampled choice.
               next-state (next-state state entry)
               ;; Reconstruct the current state through transition back
               ;; from the next state; the rdb will be different.
               prev-state (prev-state state next-state entry)
               ;; Apply Metropolis-Hastings acceptance rule to select
               ;; either the new or the current state.
               state (if (> (- (utility next-state) (utility prev-state))
                            (Math/log (rand)))
                       next-state
                       state)]
           ;; Include the selected state into the sequence of
           ;; samples, setting the weight to the unit weight if the
           ;; sample is in the support of the target distribution,
           ;; -Infinity otherwise.
           (cons (correct-log-weight state) (sample-seq state)))))]

    (let [state (:state (exec ::algorithm prog value (assoc initial-state ::alpha alpha ::sigma sigma)))]
      (if (seq (state ::trace))
        (sample-seq state)
        ;; No randomness in the program.
        (repeat (correct-log-weight state))))))
