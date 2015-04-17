(ns pdia
  (:require [clojure.string :as str :refer [join]]
            [clojure.java.io :as io])
  (:use [anglican emit runtime]
        [anglib crp]))

(defn read-data
  "reads text resource as data"
  [data-source]
  (-> (slurp (io/resource data-source))
      str/lower-case
      (str/replace #"[^a-z \n]" "")
      (str/replace #"[ \n]+" " ")
      seq))

(with-primitive-procedures [read-data join]
  (defquery pdia
    "Probabilistic Deterministic Infinite Automata"
    data-source
    (let [data (if data-source 
                 (read-data (str data-source))
                 '(\A \B \B \A \B \A \B \B \A \B \A \B \B))
          vocabulary (distinct data)
          model-prior (dirichlet (repeat (count vocabulary) 1.))

          alpha 10.
          top-level (crp alpha)

          alpha0 10.
          symbol->state-generator
          (mem (fn [symbol] (DPmem alpha0 top-level)))

          state-symbol->next-state
          (mem (fn [state symbol] ((symbol->state-generator symbol))))

          state->observation-model
          (mem (fn [state] (sample model-prior)))

          observation-noise 
          (mem (fn [state]
                 (categorical (map vector
                                   vocabulary
                                   (state->observation-model state)))))
          sample-words
          (fn sample-words [state]
            (when (sample* (flip 0.9))
              (let [word (sample* (observation-noise state))]
                (conj
                  (sample-words (state-symbol->next-state state word))
                  word))))]

      (reduce (fn [state symbol]
                (observe (observation-noise state) symbol)
                (state-symbol->next-state state symbol))
              0 data)

      (predict 'words (join (sample-words 0))))))
