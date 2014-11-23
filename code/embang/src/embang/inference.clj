(ns embang.inference
  (:require [clojure.data.json :as json]))

;;; Output

(defmulti print-predict (fn [_ _ _ format] format))

(defmethod print-predict :clojure [label value weight format]
  (prn [label value weight]))

(defmethod print-predict :json [label value weight format]
  (json/pprint [(str label) value weight])
  (prn))
  
(defn print-predicts
  "print predicts as returned by a probabilistic program
  in the specified format"
  [{:keys [predicts log-weight]} output-format]
  (doseq [[name value] predicts]
    (print-predict name value (Math/exp log-weight) output-format)))
