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
  [max-particle-count]
  (into embang.state/initial-state
        {::max-particle-count        ; max number of running threads
         max-particle-count       

         ;;; Shared state
         ::particle-count (atom 0)   ; number of running particles
         ::particle-queue            ; queue of launched particles
         (atom clojure.lang.PersistentQueue/EMPTY)
         ::average-weights (atom {}) ; average weights and counts

         ::multiplier 1              ; number of collapsed particles
         
         ;;; Maintaining observe identifiers.
         ::observe-counts {}
         ::observe-last-id nil}))

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

(defn observe-id
  "returns an unique idenditifer for observe and the updated state"
  [obs state]
  (checkpoint-id obs state ::observe-counts ::observe-last-id))

(defmethod checkpoint [::algorithm embang.trap.observe] [_ obs]
  (let [;; Incorporate new observation
        state (add-log-weight (:state obs)
                              (observe (:dist obs) (:value obs)))

        ;; Compute unique observe-id of this observe,
        ;; required for non-global observes.
        [observe-id state] (observe-id obs state)

        ;; Update average weight for this barrier.
        log-weight (get-log-weight state)
        weight (Math/exp log-weight)
        average-weight (average-weight! state observe-id weight)
        weight-ratio (if (pos? average-weight)
                       (/ weight average-weight)
                       1.)

        ;; Compute multiplier and new weight.
        ceil-ratio (Math/ceil weight-ratio)
        floor-ratio (- ceil-ratio 1.)
        [multiplier new-log-weight]
        (if (> (- ceil-ratio weight-ratio) (rand))
          [(bigint floor-ratio) (- log-weight
                                   (Math/log floor-ratio))]
          [(bigint ceil-ratio) (- log-weight
                                  (Math/log ceil-ratio))])]

    (if (zero? multiplier)
      (do (swap! (state ::particle-count) dec) nil)
      ;; Continue the thread as well as add
      ;; more threads if the multiplier is greater than 1.
      (let [state (set-log-weight state new-log-weight)]
        (loop [multiplier multiplier]
          (cond
            (= multiplier 1)
            ;; Last particle to add, continue in the current thread.
            #((:cont obs) nil state)

            (>= @(state ::particle-count)
                (state ::max-particle-count))
            ;; No place to add more particles, collapse remaining
            ;; particles into the current particle.
            #((:cont obs) nil (update-in state [::multiplier]
                                         * multiplier))

            :else
            ;; Launch new thread.
            (let [new-thread (future (exec ::algorithm
                                           (:cont obs) nil state))]
              (swap! (state ::particle-count) inc)
              (swap! (state ::particle-queue) #(conj % new-thread))
              (recur (dec multiplier)))))))))

(defmethod checkpoint [::algorithm embang.trap.result] [_ res]
  (swap! ((:state res) ::particle-count) dec)
  res)

(defmethod infer :pcascade [_ prog & {:keys [number-of-threads]
                                      :or {number-of-threads 2}}]
  (let [initial-state (make-initial-state number-of-threads)]
    (letfn
      [(sample-seq []
         (lazy-seq
           (when (zero? @(initial-state ::particle-count))
             ;; All existing particles finished, launch new particles.
             (let [new-threads (repeatedly
                                 number-of-threads
                                 #(future
                                    (exec ::algorithm
                                          prog nil initial-state)))]
                 (swap! (initial-state ::particle-count)
                        #(+ % number-of-threads))
                 (swap! (initial-state ::particle-queue)
                        #(into % new-threads))))

           (if (empty? @(initial-state ::particle-queue))
             ;; The queue is empty, all of the particles
             ;; we have just added died midway. Add more
             ;; on the next round.
             (sample-seq)

             ;; Retrieve first particle in the queue.
             (let [res @(peek @(initial-state ::particle-queue))]
               (swap! (initial-state ::particle-queue) pop)
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
