(ns anglican.palmh
  "Parallel Adaptive LMH
   Options:
     :number-of-threads (2 by default) - number of threads"
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:require [anglican.state :as state]
            [anglican.almh :refer [prev-state next-state
                                   select-entry utility award
                                   update-choice-count
                                   add-choice-predicts
                                   initial-state]])
  (:use #?(:clj anglican.inference
          :cljs [anglican.inference :only [infer exec]])
        [anglican.lmh :only [accept? get-trace correct-log-weight]]
        #?(:clj [anglican.plmh :only [ncall]])))

;;;; Parallel Adaptive Lightweight Metropolis-Hastings

(derive ::algorithm :anglican.almh/algorithm)

#?(:clj
  (defn next-state-seq
    "returns lazy sequence of next states"
    [state number-of-threads]
    (ncall #(let [entry (select-entry state)]
             [entry (next-state state entry)])
           number-of-threads)))

#?(:clj
  (defmethod infer :palmh [_ prog value
                           & {:keys [number-of-threads
                                     predict-choices] ; report stats
                              :or {number-of-threads 2
                                   predict-choices false}}]
    (letfn
        [(next-seq [state] (next-state-seq state number-of-threads))
         (sample-seq [state next-states]
           (lazy-seq
            (let [;; Compute next state from the resampled choice.
                  [entry next-state] (first next-states)
                  ;; Reconstruct the current state through transition
                  ;; back from the next state; the rdb will be different.
                  prev-state (prev-state state next-state entry)
                  [state next-states]
                  ;; Apply Metropolis-Hastings acceptance rule to select
                  ;; either the new or the current state.
                  (if (accept? (utility next-state entry)
                               (utility prev-state entry))
                    ;; The new state is accepted --- award choices
                    ;; according to changes in predicts to favor
                    ;; choices which affect more predicts.
                    (let [next-state (award next-state entry)]
                      [next-state (next-seq next-state)])
                    ;; The old state is held.
                    [state (rest next-states)])

                  ;; In any case, update the entry count.
                  state (update-choice-count state entry)

                  ;; Include the selected state into the sequence of
                  ;; samples, setting the weight to the unit weight.
                  sample (correct-log-weight state)
                  ;; Optionally, add rewards and counts to predicts.
                  sample (if predict-choices
                           (add-choice-predicts sample)
                           sample)]
              (cons sample (sample-seq state next-states)))))]

      (let [state (:state (exec ::algorithm prog value initial-state))]
        (if (seq (get-trace state))
          (sample-seq state (next-seq state))
          ;; No randomness in the program.
          (repeat (correct-log-weight state)))))))
