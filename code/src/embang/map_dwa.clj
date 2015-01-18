(ns embang.map-dwa
  (:use [embang.state :exclude [initial-state]]
        embang.inference
        [embang.runtime :only [observe]]
        [embang.map :exlcude [distance-heuristic]]))

;;; Dynamic Weighing A* MAP search

(derive ::search :embang.map/search)

(defn distance-heuristic 
  [smp value belief]
  ;; TODO
  0)

;;; Inference method 

(defmethod infer :map-dwa
  [_ prog & args]
  (apply infer :map prog 
         :distance-heuristic distance-heuristic
         args))
