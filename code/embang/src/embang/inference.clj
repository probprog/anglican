(ns embang.inference
  (:require [clojure.data.json :as json])
  (:use embang.state
        [embang.runtime :only [sample observe]]))

;;; Checkpoints

(defmulti checkpoint (juxt type identity))

(defmethod checkpoint [embang.trap.observe ::algorithm] [obs algorithm]
  #((:cont obs) nil (add-weight (:state obs)
                                (observe (:dist obs) (:value obs)))))

(defmethod checkpoint [embang.trap.sample ::algorithm] [smp algorithm]
  (let [value (sample (:dist smp))]
    #((:cont smp) value (add-weight (:state smp)
                                    (observe (:dist smp) value)))))

(defmethod checkpoint [embang.trap.result ::algorithm] [res algorithm]
  (:state res))

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
