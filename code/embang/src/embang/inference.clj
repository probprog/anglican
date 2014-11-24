(ns embang.inference
  (:require [clojure.string :as str]
            [clojure.data.json :as json]
            embang.trap)
  (:use embang.state
        [embang.runtime :only [sample observe]]))

;; Inference multimethod

(defmulti infer (fn [algorithm & _] algorithm))

;;; Checkpoints

(defmulti checkpoint (fn [cpt alg] [(type cpt) alg]))

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

(defmethod print-predict :anglican [label value weight format]
  (println (str/join "," [label value weight])))

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
