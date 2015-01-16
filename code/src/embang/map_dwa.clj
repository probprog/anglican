(ns embang.map-dwa
  (:use [embang.state :exclude [initial-state]]
        embang.inference
        [embang.runtime :only [observe]]
        [embang.map :exclude [distance-heuristic]]))

;;; Dynamic Weighing A* MAP search

(derive ::search :embang.map/search)

(defn distance-heuristic
  "returns greedy guess distance heuristic"
  [smp value]
  ;; TODO
  0)

(defmethod expand [::search embang.trap.sample] [smp ol]
  ;; A sample node is expanded by inserting all of the
  ;; child nodes into the open list. The code partially
  ;; repeats the code of checkpoint [::algorithm sample].
  (let [state (:state smp)
        id (bandit-id smp state)
        bandit ((state :embang.map/bandits) id)
        ol (reduce
             ;; For every child of the latent variable
             ;; in the constructed subgraph of G_prog:
             (fn [ol [value _]]
               ;; Update the state and the trace ...
               (let [past-reward (get-log-weight state)
                     state (-> state
                               (add-log-weight
                                 ;; The log-weight is truncated at 0
                                 ;; to avoid divergence.
                                 (min 0. (observe (:dist smp) value)))
                               (record-random-choice id value past-reward))
                     ;; ... and compute cost estimate till
                     ;; the termination.
                     f (+ (- past-reward) (distance-heuristic smp value))]
                 ;; If the distance estimate is 
                 ;; a meaningful number, insert the node
                 ;; into the open list.
                 (if-not (Double/isNaN f)
                   (ol-insert ol
                              (->node
                                #(exec ::search (:cont smp) value state)
                                f))
                   ol)))
             ol (seq (:arms bandit)))]
    ;; Finally, remove and expand the next node 
    ;; from the open list.
    (next-node ol)))

;;; Inference method 

(defmethod infer :map-dwa
  [_ prog & args]
  (binding [*search* ::search]
    (apply infer :map prog args)))
