(ns embang.pcascade
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:use [embang.state :exclude [initial-state]]
        embang.inference
        [embang.runtime :only [observe sample]]))

;;;; Parallel Cascade

(derive ::algorithm :embang.inference/algorithm)

;;; Initial state

(def initial-state
  "initial state for Parallel Cascade"
  (into embang.state/initial-state
        {;; Algorithm parameter --- maximum size of thread queue
         ::max-count nil

         ;; Shared state
         ::queue (atom clojure.lang.PersistentQueue/EMPTY)
         ::average-weights (atom {})
         
         ;; Number of particles exceeding max-count
         ;; if created
         ::multiplier 1}))

(defn average-weight!
  "updates and returns updated average weight for given id"
  [state id weight]
  (if (contains? @(state ::average-weights) id)
    ;; The id is in the table, update the average weight.
    (let [[average-weight _]
          (swap! ((state ::average-weights) id)
                 (fn [[average-weight count]]
                   [(/ (+ (* count average-weight) 
                          (* weight (double (state ::multiplier))))
                       (double (+ count (state ::multiplier))))
                    (double (+ count (state ::multiplier)))]))]
      average-weight)

    ;; First particle arriving at this id ---
    ;; initialize the average weight.
    (average-weight!
      (update-in state [::average-weights]
                 (fn [average-weights]
                   (assoc average-weights id (atom [0. 0]))))
      id weight)))

(defmethod checkpoint [::algorithm embang.trap.observe] [_ obs]
  (let [;; Incorporate new observation
        state (add-log-weight (:state obs)
                              (observe (:dist obs) (:value obs)))
        
        ;; Update average weight for this barrier.
        weight (Math/exp (get-log-weight (:state obs)))
        average-weight (average-weight! state (:id obs) weight)
        weight-ratio (if (pos? average-weight)
                       (/ weight average-weight)
                       1.)

        ;; Compute multiplier and new weight.
        floor-ratio (Math/floor weight-ratio)
        ceil-ratio (+ 1. floor-ratio)
        [multiplier new-weight]
        (if (< (- weight-ratio floor-ratio) (rand))
          [(int floor-ratio) (/ weight floor-ratio)] 
          [(int ceil-ratio) (/ weight ceil-ratio)])
        new-log-weight (Math/log new-weight)]

    ;; If the multiplier is zero, die and return nil.
    (when (pos? multiplier)
      (let [state (set-log-weight state new-log-weight)]
        (loop [multiplier multiplier]
          (cond
            (= multiplier 1) 
            ;; Last particle to add, continue in the current thread
            #((:cont obs) nil state)

            (>= (count @(state ::queue)) (state ::max-count))
            ;; No place to add more particles, multiply the 
            ;; current particle.
            #((:cont obs) nil (update-in state ::multiplier 
                                         (fn [m] (* m multiplier))))

            :else
            (do
              ;; Launch new thread.
              (swap! (state ::queue)
                     #(conj % (future ((:cont obs) nil state))))
              (recur (dec multiplier)))))))))


