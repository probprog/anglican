(ns lda
  (:use [anglican emit runtime]
        psychreview-data))

(defm subset 
  "subsets psychreview-data keeping ndocs first documents"
  [ndocs]
  (let [docs (set (range ndocs))]
    (filter (fn [[doc pos word]]
              (contains? docs doc))
            psychreview-data)))

(defproc DSD*
  "discrete-symmetric-dirichlet process"
  [alpha N] [counts (vec (repeat N (double alpha)))]
  (produce [this] (discrete counts))
  (absorb [this sample]
    (DSD* alpha N (update-in counts [sample] + 1.))))

;; Here is an alternative way of wrapping a primitive procedure.
;; Instead of declaring DSD as primitive for defquery, we define
;; the process using an auxiliary name DSD*, and then define DSD
;; as an m! function.
(with-primitive-procedures [DSD*]
  (defm DSD 
    "discrete-symmetric-dirichlet process"
    [& args] (apply DSD* args)))

;; Now DSD can be used just as a random process defined in the
;; runtime.

(defquery lda 
  "Latent Dirichlet Allocation on PsychReview"
  ndocs 
  (let [ndocs (or ndocs 1281)
        corpus (subset ndocs)
        K 50
        V 9244
        alpha 1.0
        beta 0.01
        topics-prior (DSD alpha K)
        words-prior (DSD beta V)]

    ;; produce-observe-absorb loop
    (loop [words corpus
           topic-dists {}
           word-dists {}]
      (if (seq words)
        (let [[[doc pos word] & words] words
              topic-dist (get topic-dists doc topics-prior)
              topic (sample (produce topic-dist))
              word-dist (get word-dists topic words-prior)]
          (observe (produce word-dist) word)
          (recur words 
                 (assoc topic-dists doc
                        (absorb topic-dist topic))
                 (assoc word-dists topic
                        (absorb word-dist word))))

        ;; predict topics
        (reduce
          (fn [_ doc]
            (predict doc (sample*
                           (produce (get topic-dists doc)))))
          () (range ndocs))))))
