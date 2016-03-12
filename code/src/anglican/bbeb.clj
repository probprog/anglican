(ns anglican.bbeb
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:use [anglican
         state inference bbvb
         [runtime :only [observe sample]]]))

;;; Black-box empirical bayes (BBEB)

(derive ::algorithm :anglican.bbvb/algorithm)

(defmethod checkpoint [::algorithm anglican.trap.sample] [_ smp]
  (let [[address state] (get-address smp)
        prior (:dist smp)
        ignore (ignore? state prior)]
    (if ignore
      #((:cont smp) (sample prior) state)
      (let [proposal (first (get-or-create-q! state address (:dist smp)))
            value (sample proposal)
            state (assoc-gradient state address proposal value)]
        #((:cont smp) value state)))))

(defmethod infer :bbeb [_ prog value
                        & {:keys [only
                                  number-of-particles
                                  base-stepsize
                                  robbins-monro
                                  adagrad
                                  initial-proposals]
                           :or {only nil
                                number-of-particles 100
                                base-stepsize 1.0
                                robbins-monro 0.0
                                adagrad true
                                initial-proposals nil}}]
  (assert (>= number-of-particles 1)
          ":number-of-particles must be at least 1")
  (let [initial-state (make-initial-state only)]
    (when initial-proposals
      (merge-q! initial-state initial-proposals))
    (letfn
      [(sample-seq
        [steps]
        (lazy-seq
         ;; Run a new sweep.
         (let [next-particles
               (loop [particles (repeatedly
                                 number-of-particles
                                 #(exec ::algorithm
                                        prog value
                                        initial-state))]
                 (if (every? #(instance? anglican.trap.result %) particles)
                   (do
                     (update-proposals! particles
                                        (/ base-stepsize (Math/pow (inc steps) robbins-monro))
                                        adagrad)
                     particles)
                   (let [results (filter #(instance? anglican.trap.result %) particles)
                         observes (filter #(instance? anglican.trap.observe %) particles)]
                     (assert (= (count particles)
                                (+ (count results)
                                   (count observes))))
                     (recur (concat results
                                    (map #(exec ::algorithm
                                                (:cont %) nil
                                                (:state %))
                                         observes))))))]
           (concat (map :state next-particles)
                   (sample-seq (inc steps))))))]
      ;; initialize
      (sample-seq 1))))
