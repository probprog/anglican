(ns embang.pcascade
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:use [embang.state :exclude [initial-state]]
        embang.inference
        [embang.runtime :only [observe sample]]))

;;;; Parallel Cascade

(derive ::algorithm :embang.inference/algorithm)

;;; Initial state

(defn make-initial-state
  "initial state constructor for Parallel Cascade, parameterized
  by the maximum number of running threads"
  [max-count]
  (into embang.state/initial-state
        {;; maximum number of running threads
         ::max-count max-count

         ;;; Shared state
         ;; Number of running threads
         ::count (atom 0)
         ;; Queue of running threads (futures)
         ::queue (atom clojure.lang.PersistentQueue/EMPTY)
         ;; Table of average weights and hit counts
         ::average-weights (atom {})

         ;; Number of collapsed particles
         ::multiplier 1}))

(defn average-weight!
  "updates and returns updated average weight for given id"
  [state id weight]
  (when-not (contains? @(state ::average-weights) id)
    ;; First particle arriving at this id ---
    ;; initialize the average weight.
    (swap! (state ::average-weights) #(assoc % id (atom [0. 0]))))
  ;; The id is in the table, update the average weight.
  (let [[average-weight _]
        (swap! (@(state ::average-weights) id)
               (fn [[average-weight count]]
                 [(/ (+ (* count average-weight)
                        (* weight (double (state ::multiplier))))
                     (double (+ count (state ::multiplier))))
                  (double (+ count (state ::multiplier)))]))]
    average-weight))

(defmethod checkpoint [::algorithm embang.trap.observe] [_ obs]
  (let [;; Incorporate new observation
        state (add-log-weight (:state obs)
                              (observe (:dist obs) (:value obs)))

        ;; Update average weight for this barrier.
        weight (Math/exp (get-log-weight state))
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
    (if (zero? multiplier)
      (do (swap! (state ::count) dec) nil)
      ;; Otherwise, continue the thread as well as add
      ;; more threads if the multiplier is greater than 1.
      (let [state (set-log-weight state new-log-weight)]
        (loop [multiplier multiplier]
          (cond
            (= multiplier 1)
            ;; Last particle to add, continue in the current thread.
            #((:cont obs) nil state)

            (>= @(state ::count) (state ::max-count))
            ;; No place to add more particles, collapse remaining
            ;; particles into the current particle.
            #((:cont obs) nil (update-in state [::multiplier]
                                         (fn [m] (* m multiplier))))
            :else
            ;; Launch new thread.
            (let [new-thread (future (exec ::algorithm
                                            (:cont obs) nil state))]
              (swap! (state ::count) inc)
              (swap! (state ::queue) #(conj % new-thread))
              (recur (dec multiplier)))))))))

(defmethod checkpoint [::algorithm embang.trap.result] [_ res]
  (swap! ((:state res) ::count) dec)
  res)

(defmethod infer :pcascade [_ prog & {:keys [number-of-threads]
                                      :or {number-of-threads 2}}]
  (let [initial-state (make-initial-state number-of-threads)]
    (letfn
      [(sample-seq []
         (lazy-seq
           (if (empty? @(initial-state ::queue))
             ;; All existing particles died, launch new particles.
             (let [new-threads (repeatedly
                                 number-of-threads
                                 #(future
                                    (exec ::algorithm
                                          prog nil initial-state)))]
                 (swap! (initial-state ::count) #(+ % number-of-threads))
                 (swap! (initial-state ::queue) #(into % new-threads))
                 (sample-seq))

             ;; Retrieve first particle in the queue.
             (let [res @(peek @(initial-state ::queue))]
               (swap! (initial-state ::queue) pop)
               (if (some? res)
                 ;; The particle has lived through to the result.
                 ;; Multiply the weight by the multiplier.
                 (let [state (add-log-weight
                               (:state res)
                               (Math/log
                                 (double
                                   ((:state res) ::multiplier))))]
                   ;; Add the state to the output sequence.
                   (cons state (sample-seq)))
                 ;; The particle died midway, retrieve the next one.
                 (sample-seq))))))]
      (sample-seq))))
