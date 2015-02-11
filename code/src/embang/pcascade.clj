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
  [particle-cap]
  (into embang.state/initial-state
        {::particle-cap particle-cap ; max number of running threads

         ;;; Shared state
         ::particle-count (atom 0)   ; number of running particles
         ::particle-queue            ; queue of launched particles
         (atom clojure.lang.PersistentQueue/EMPTY)
         ::average-weights (atom {}) ; average weights

         ::multiplier 1              ; number of collapsed particles
         
         ;;; Maintaining observe identifiers.
         ::observe-counts {}
         ::observe-last-id nil}))

;; Average weights are stored as tuples [log-total-weight count].

(defn ^:private log-sum
  "computes (log (+ (exp x) (exp y))) safely"
  [log-x log-y]
  (let [log-max (max log-x log-y)]
    (if (< (/ -1. 0.) log-max (/ 1. 0.))
      (+ log-max
         (Math/log (+ (Math/exp (- log-x log-max))
                      (Math/exp (- log-y log-max)))))
      log-max)))

(defn weight-ratio!
  "updates average weight for observe-id and returns weight-ratio"
  [state observe-id log-weight mplier]
  (when-not (contains? @(state ::average-weights) observe-id)
    ;; First particle arriving at this id ---
    ;; initialize the average weight.
    (swap! (state ::average-weights)
           #(assoc % observe-id (atom [(/ -1. 0.) 0]))))

  ;; The id is in the table, update the average weight.
  (let [[log-total cnt]
        (swap! (@(state ::average-weights) observe-id)
               (fn [[log-total cnt]]
                 [(log-sum log-total (+ log-weight (Math/log mplier)))
                  (+ cnt mplier)]))]
    (if (= log-total (/ -1. 0.)) 1.    ; all particles had 0 weight
      (Math/exp (- log-weight (- log-total (Math/log cnt)))))))

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
        multiplier (state ::multiplier)
        weight-ratio (weight-ratio! state observe-id
                                    log-weight multiplier)

        ;; Compute log weight and multiplier.
        ceil-ratio (Math/ceil weight-ratio)
        floor-ratio (- ceil-ratio 1.)
        [log-weight multiplier]
        (if (< (- weight-ratio floor-ratio) (rand))
          [(- log-weight (Math/log floor-ratio)) (bigint floor-ratio)]
          [(- log-weight (Math/log ceil-ratio)) (bigint ceil-ratio)]) ]

    (if (zero? multiplier)
      (do (swap! (state ::particle-count) dec) nil)
      ;; Continue the thread as well as add
      ;; more threads if the multiplier is greater than 1.
      (let [state (set-log-weight state log-weight)]
        (loop [multiplier multiplier]
          (cond
            (= multiplier 1)
            ;; Last particle to add, continue in the current thread.
            #((:cont obs) nil state)

            (>= @(state ::particle-count) (state ::particle-cap))
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

(defn add-cascade-predicts
  "adds internal cascade statistics as predicts"
  [state]
  (-> state 
      (add-predict '$particle-count @(state ::particle-count))
      (add-predict '$multiplier (state ::multiplier))
      (add-predict '$particle-queue-length
                   (count @(state ::particle-queue)))))

(defmethod infer :pcascade [_ prog & {:keys [number-of-threads
                                             predict-cascade]
                                      :or {number-of-threads 16
                                           predict-cascade false}}]
  (let [initial-state (make-initial-state number-of-threads)]
    (letfn
      [(sample-seq []
         (lazy-seq
           (if (empty? @(initial-state ::particle-queue))
             ;; All particles died, launch new particles.
             (let [new-threads (repeatedly
                                 ;; Leave space for spawned particles.
                                 (int (Math/ceil
                                        (/ number-of-threads 2)))
                                 #(future
                                    (exec ::algorithm
                                          prog nil initial-state)))]
                 (swap! (initial-state ::particle-count)
                        #(+ % (count new-threads)))
                 (swap! (initial-state ::particle-queue)
                        #(into % new-threads))
                 (sample-seq))

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
                                   ((:state res) ::multiplier))))
                       state (if predict-cascade
                               (add-cascade-predicts state)
                               state)]
                   ;; Add the state to the output sequence.
                   (cons state (sample-seq)))
                 ;; The particle died midway, retrieve the next one.
                 (sample-seq))))))]
      (sample-seq))))
