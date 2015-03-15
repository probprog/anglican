(ns lda
  (:use [embang emit runtime]
        psychreview-data))

(defm subset 
  "subsets psychreview-data keeping ndocs first documents"
  [ndocs]
  (let [docs (set (range ndocs))]
    (filter (fn [[doc pos word]]
              (contains? docs doc))
            psychreview-data)))

(defn DSD
  "discrete-symmetric-dirichlet process"
  ([alpha N] (DSD (vec (repeat N (double alpha)))))
  ([counts]
     {:pre [(vector? counts)]}
     (reify
       random-process
       (produce [this]
         (discrete counts))
       (absorb [this sample]
         (DSD (update-in counts [sample] + 1.))))))

(with-primitive-procedures [DSD]
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
           () (range ndocs)))))))
