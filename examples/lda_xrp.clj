(ns lda-xrp
  (:use [embang emit runtime]
        [anglib xrp]
        lda))

(with-primitive-procedures [DSD]
  (defquery lda-xrp
    "Latent Dirichlet Allocation on PsychReview,
    with exchangleable random procedures"
    ndocs 
    (let [ndocs (or ndocs 1281)
          corpus (subset ndocs)
          K 50
          V 9244
          alpha 1.0
          beta 0.01
          get-topic-dist (mem (fn [doc] (XRP (DSD alpha K))))
          get-word-dist (mem (fn [topic] (XRP (DSD beta V))))]

      ;; produce-observe-absorb loop
      (loop [words corpus]
        (if (seq words)

          (let [[[doc pos word] & words] words
                topic-dist (get-topic-dist doc)
                topic (SAMPLE topic-dist)
                word-dist (get-word-dist topic)]
            (OBSERVE word-dist word)
            (recur words)))

          (loop [docs (range ndocs)]
            (if (seq docs)
              (let [[doc & docs] docs]
                (predict doc (SAMPLE* (get-topic-dist doc)))
                (recur docs))))))))
