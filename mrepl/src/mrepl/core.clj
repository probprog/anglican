(ns mrepl.core
  (:require [embang
             [inference :refer [infer warmup]]
             [core :refer [load-algorithm]]
             [state :as state]]))

;; Inference query

(defn doquery
  "performs inference query;
  returns lazy sequence of states"
  [algorithm program value & options]
  (do
    ;; Use the auto-loading machine in embang.core to load
    ;; the inference algorithm on demand.
    (load-algorithm algorithm)
    ;; Then, call the inference to create a lazy sequence
    ;; of states.
    (apply infer algorithm (warmup program value) nil options)))

;; State readers

(def get-predicts (comp (partial into {}) state/get-predicts))
(def get-log-weight state/get-log-weight)
