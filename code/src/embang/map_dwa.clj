(ns embang.map-dwa
  (:use [embang.state :exclude [initial-state]]
        embang.inference
        [embang.runtime :only [observe]]
        embang.map))

;;; Dynamic Weighing A* MAP search

(derive ::search :embang.map/search)

(defmethod distance-heuristic ::search
  [smp value belief]
  ;; TODO
  0)

;;; Inference method 

(defmethod infer :map-dwa
  [_ prog & args]
  (binding [*search* ::search]
    (apply infer :map prog args)))
