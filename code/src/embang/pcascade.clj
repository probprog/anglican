(ns embang.pcascade
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:use [embang.state :exclude [initial-state]]
        embang.inference
        [embang.runtime :only [log-sum-exp observe sample]]))

;;;; Parallel Cascade

(derive ::algorithm :embang.inference/algorithm)

;;; Initial state

(defn make-initial-state
  "initial state constructor for Parallel Cascade, parameterized
  by the maximum number of running threads"
  [particle-cap]
  (into embang.state/initial-state
        {::particle-cap particle-cap ; max number of running threads
         ::particle-id nil           ; unique id, for monitoring;
                                     ; assigned on thread launch
         ;;; Shared state
         ::particle-next-id (atom 0) ; shared source of particle ids
         ::particle-count (atom 0)   ; number of running particles
         ::sample-queue              ; queue of produced samples
         (atom clojure.lang.PersistentQueue/EMPTY)
         ::average-weights (atom {}) ; average weights

         ::multiplier 1N             ; number of collapsed particles
         
         ;;; Maintaining observe identifiers.
         ::observe-counts {}
         ::observe-last-id nil}))

;; Average weights are stored as tuples [log-total-weight count].

(defn weight-ratio!
  "updates average weight for observe-id and returns weight-ratio"
  [state observe-id log-weight mplier]
  (when-not (contains? @(state ::average-weights) observe-id)
    ;; First particle arriving at this observe-id ---
    ;; initialize the average weight.
    (swap! (state ::average-weights)
           #(assoc % observe-id (atom [(/ -1. 0.) 0]))))

  ;; The observe-id is in the table, update the average weight.
  (let [[log-total cnt]
        (swap! (@(state ::average-weights) observe-id)
               (fn [[log-total cnt]]
                 [(log-sum-exp log-total
                               (+ log-weight (Math/log mplier)))
                  (+ cnt mplier)]))]
    (if (= log-total (/ -1. 0.)) 1.    ; all particles had 0 weight
      (Math/exp (- log-weight (- log-total (Math/log cnt)))))))

(defn observe-id
  "returns an unique idenditifer for observe and the updated state"
  [obs state]
  (checkpoint-id obs state ::observe-counts ::observe-last-id))

(defn launch-particle
  "launch a particle in a new thread"
  [cont value state]
  (future (exec ::algorithm cont value
                (assoc state ::particle-id 
                       (swap! (state ::particle-next-id) inc)))))

(defmethod checkpoint [::algorithm embang.trap.observe] [_ obs]
  (let [;; Incorporate new observation
        state (add-log-weight (:state obs)
                              (observe (:dist obs) (:value obs)))

        ;; Compute unique observe-id of this observe,
        ;; required for non-global observes.
        [observe-id state] (observe-id obs state)

        ;; Compute weight ratio.
        log-weight (get-log-weight state)
        multiplier (state ::multiplier)
        weight-ratio (weight-ratio! state observe-id
                                    log-weight multiplier)

        ;; Compute log weight and multiplier.
        ceil-ratio (Math/ceil weight-ratio)
        ratio (if (< (- ceil-ratio weight-ratio) (rand))
                ceil-ratio (- ceil-ratio 1.))
        log-weight (- log-weight (Math/log ratio))
        multiplier (bigint ratio)]

    (if (zero? multiplier)
      ;; If the multiplier is 0, stop the thread and return nil.
      (do (swap! (state ::particle-count) dec)
          nil)
      ;; Otherwise, continue the thread as well as add
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
            (let [new-thread (launch-particle (:cont obs) nil state)]
              (swap! (state ::particle-count) inc)
              (recur (dec multiplier)))))))))

(defmethod checkpoint [::algorithm embang.trap.result] [_ res]
  (let [state (:state res)
        ;; Multiply the weight by the multiplier.
        state (add-log-weight
               state (Math/log (double (state ::multiplier))))]
    (swap! (state ::particle-count) dec)
    (swap! (state ::sample-queue) conj state))
  res)

(defn add-cascade-predicts
  "adds internal cascade statistics as predicts"
  [state]
  (-> state
      (add-predict '$particle-id (state ::particle-id))
      (add-predict '$particle-count @(state ::particle-count))
      (add-predict '$multiplier (state ::multiplier))))

(defmethod infer :pcascade [_ prog value
                            & {:keys [number-of-threads
                                      predict-cascade]
                               :or {number-of-threads 16
                                    predict-cascade false}}]
  (let [initial-state (make-initial-state number-of-threads)]
    (letfn
        [(sample-seq []
           (lazy-seq
            (if (empty? @(initial-state ::sample-queue))
              ;; No ready samples.
              (if (zero? @(initial-state ::particle-count))
                ;; All particles died, launch new particles.
                (let [new-threads
                      (repeatedly
                       ;; Leave space for spawned particles.
                       (int (Math/ceil
                             (/ number-of-threads 2)))
                       #(launch-particle prog value initial-state))]
                  (swap! (initial-state ::particle-count)
                         #(+ % (count new-threads)))
                  (sample-seq))
                ;; Particles are still running, wait for them
                (do
                  (Thread/yield)
                  (sample-seq)))

              ;; Retrieve first sample from the queue.
              (let [state (peek @(initial-state ::sample-queue))]
                (swap! (initial-state ::sample-queue) pop)
                ;; Add the state to the output sequence.
                (cons (if predict-cascade
                              (add-cascade-predicts state)
                              state)
                  (cons state (sample-seq)))))))]
      (sample-seq))))
