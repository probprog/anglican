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
    ;; Optionally, warm up the program by pre-evaluating
    ;; the determenistic prefix.
    (let [options* (apply hash-map options)
          [program value] (if (:warmup options* true)
                            [(warmup program value) nil]
                            [program value])]
      ;; Finally, call the inference to create
      ;; a lazy sequence of states.
      (try
        (apply infer algorithm program value options)
        (catch Exception e
          (when (:debug options*)
            (.printStackTrace e *out*))
          (throw e))))))

;; State readers

(def get-predicts (comp (partial into {}) state/get-predicts))
(def get-log-weight state/get-log-weight)
