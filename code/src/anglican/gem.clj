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

      ;; Since sampling from a distribution object is
      ;; unweighted, the random source for the distribution is
      ;; an equalized sequence of inferred samples.  Sampling is
      ;; imlemented by removing the first element from a lazy
      ;; sequence of equalized samples.

      (let [;; Random source is a mutable reference.
            source (-> (apply infer algorithm
                              query value options)
                       equalize
                       ref)
            ;; Next sample is the first sample removed
            ;; from the lazy sequence in the source.
            next-sample #(dosync
                           (let [[sample & samples] @source]
                             (ref-set source samples)
                             sample))]
      (reify distribution

        ;; A sample from the distribution is the collection
        ;; of predicts in a single sample from the inferred
        ;; sample sequence.
        (sample [this]
          (get-predicts (next-sample)))

        ;; Observing a value requires source code analysis,
        ;; not implemented yet. For a future implementation,
        ;; the source code of query is in the meta-data for
        ;; key `:source'.
        (observe [this value]
          (throw (Exception. "not implemented"))))))))
