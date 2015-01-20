(ns embang.asmh
  (:use [embang.state :exclude [initial-state]]
        embang.inference
        [embang.lmh :exclude [initial-state utility]]))

;;;; Adaptive scheduling single-site Metropolis-Hastings

(derive ::algorithm :embang.lmh/algorithm)

;;; Initial state

(def initial-state
  "initial state for ASMH"
  (into embang.lmh/initial-state
        {}))

(defn utility
  "computes state utility, used to determine
  the acceptance log-probability as (next-utility - prev-utility)"
  [state entry]
  (+ (get-log-weight state)
     (get-log-retained state)
     (- (Math/log (count (state ::trace))))))

(defmethod infer :asmh [_ prog & {}]
  (letfn
    [(sample-seq [state]
       (lazy-seq
         (let [;; Choose uniformly a random choice to resample.
               entry (rand-nth (get-trace state))
               ;; Compute next state from the resampled choice.
               next-state (mk-next-state state entry)
               ;; Reconstruct the current state through transition back
               ;; from the next state; the rdb will be different.
               prev-state (mk-prev-state state next-state entry)
               ;; Apply Metropolis-Hastings acceptance rule to select
               ;; either the new or the current state.
               state (if (> (- (utility next-state entry)
                               (utility prev-state entry))
                            (Math/log (rand)))
                       next-state
                       state)]
           ;; Include the selected state into the sequence of samples,
           ;; setting the weight to the unit weight.
           (cons (set-log-weight state 0.) (sample-seq state)))))]
    (sample-seq (:state (exec ::algorithm prog nil initial-state)))))
