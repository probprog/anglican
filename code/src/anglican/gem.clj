(ns anglican.gem
  "higher level inference abstraction"
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (use [anglican runtime emit state inference]))

(defn conditional
  "accepts an Anglican query and returns
  a conditional distribution defined by the query"
  [query & options]
  ;; Algorithm parameters are an optional sequence of the form
  ;;   [inference-algorithm & algorithm-options]
  ;; with importance sampling as the default inference algorithm.
  (let [[algorithm & options] options
        algorithm (or algorithm :importance)]
    ;; Conditional distribution is a function which, when applied
    ;; to the values (argument of query), returns a distribution
    ;; object.
    (fn [& value]
      (let [source (-> (apply infer algorithm
                              query value options)
                       equalize
                       ref)]
      (reify distribution
        (sample [this]
          (dosync
            (let [[state & states] @source]
              (ref-set source states)
              (get-predicts state))))
        (observe [this value]
          (throw (Exception. "not implemented"))))))))
