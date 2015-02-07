(ns embang.plmh
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:use [embang.state :only [set-log-weight]]
        embang.inference
        embang.lmh))

;;;; Parallel Lightweight Metropolis-Hastings

(derive ::algorithm :embang.lmh/algorithm)

(defn ncall
  "executes thunk in n parallel threads,
  returning a lazy sequence of results"
  [thunk n]
  (letfn [(result-seq [running]
            (lazy-seq 
              (cons (deref (peek running))
                    (result-seq (conj (pop running)
                                      (future (thunk)))))))]
    (result-seq (into clojure.lang.PersistentQueue/EMPTY
                      (repeatedly n #(future (thunk)))))))

(defn next-state-seq
  "returns lazy sequence of next states"
  [state number-of-threads]
  (ncall #(let [entry (rand-nth (get-trace state))]
                [entry (next-state state entry)])
           number-of-threads))

(defmethod infer :plmh [_ prog & {:keys [number-of-threads]
                                  :or {number-of-threads 2}}]
  (letfn
    [(next-seq [state] (next-state-seq state number-of-threads))
     (sample-seq [state next-states]
       (lazy-seq
         (let [;; Compute next state from the resampled choice.
               [entry next-state] (first next-states)
               ;; Reconstruct the current state through transition
               ;; back from the next state; the rdb will be different.
               prev-state (prev-state state next-state entry)]

           ;; Apply Metropolis-Hastings acceptance rule to select
           ;; either the new or the current state.
           (if (> (- (utility next-state) (utility prev-state))
                  (Math/log (rand)))
             ;; Include the selected state into the sequence of
             ;; samples, setting the weight to the unit weight.
             (cons (set-log-weight next-state 0.)
                   (sample-seq next-state (next-seq next-state)))
             (cons (set-log-weight state 0.)
                   (sample-seq state (rest next-states)))))))]

    (let [state (:state (exec ::algorithm prog nil initial-state))]
      (if (seq (get-trace state))
        (sample-seq state (next-seq state))
        ;; No randomness in the program.
        (repeat (set-log-weight state 0.))))))
