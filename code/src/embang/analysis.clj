(ns embang.analysis
  (require [clojure.java.io :as io])
  (use [embang.results :only [parsed-line-seq
                              total-weights
                              normalize-weights]]))

;;;; Analysis of inference results

;;; Sample distance measures

(defn KL
  "computes Kullback-Leibler divergence for value frequencies."
  [p-freqs q-freqs] {:pre [(map? p-freqs) (map? q-freqs)]}
  ;; Fix freqs so that p and q have the same keys.
  (let [fix-freqs (fn [a-freqs b-freqs]
                     (reduce (fn [ac k]
                               (if (ac k) ac
                                 (assoc ac k Double/MIN_NORMAL)))
                             a-freqs (keys b-freqs)))
        p-freqs (fix-freqs p-freqs q-freqs)
        q-freqs (fix-freqs q-freqs p-freqs)]
    (reduce (fn [kl k]
              (let [q (q-freqs k)
                    p (p-freqs k)]
                (+ kl (* p (Math/log (/ p q))))))
            0. (keys q-freqs))))

;; For use with KS-two-samples:
(defn search-sorted
  "returns a sequence of indices such that if `values' are
  inserted at the indices into `grid', `grid' remains sorted;
  assumes that both `grid' and `values' are sorted"
  [grid values]
  (letfn
    [(indices [grid values index]
       (lazy-seq
         (when (seq values)
           (if (or (empty? grid)
                   (> (first grid) (first values)))
             (cons index
                   (indices grid (rest values) index))
             (indices (rest grid) values (inc index))))))]
    (indices grid values 0)))

(defn KS
  "computes Kolmogorov-Smirnov distance for two samples"
  [sa sb]
  (let [sa (sort sa)
        sb (sort sb)
        s (sort (concat sa sb))
        cdf (fn [sx]
              (let [nx (double (count sx))]
                (map #(/ (double %) nx) (search-sorted sx s))))]
    (reduce max (map #(Math/abs (- %1 %2)) (cdf sa) (cdf sb)))))

;;; Manipulating inference outputs

(defn included?
  "true when the label should be included"
  [only exclude label]
  (and (or (nil? only) (contains? only label))
       (not (contains? exclude label))))

(defn predict-seq-skipping
  "returns lazy sequence of predicts, 
  skipping first `skip' predicts"
  [skip]
  (drop skip (parsed-line-seq (line-seq (io/reader *in*)))))

;; REPL command:
(defn total-freqs
  "reads results from stdin and returns
  a table of total frequences for discrete-valued predicts"
  [& {:keys [only exclude]
      :or {only nil
           exclude #{}}}]
  (let [total-weights (total-weights)]
    (normalize-weights
      (reduce dissoc total-weights 
              (keep (complement 
                      (partial included?  only exclude))
                    (keys total-weights))))))

;; REPL command:
(defn kl-seq
  "reads results from stdin and returns a lazy sequence
  of KL distances, skipping first `skip' predict lines and
  then producing a sequence entry each `step' predict lines"
  [true-freqs & {:keys [skip step only exclude]
                 :or {skip 0
                      step 1
                      only nil
                      exclude #{}}}]
  (letfn
    [(kl-seq* [lines nlines weights]
       (lazy-seq
         (if (empty? lines) nil
           (let [[[label value weight] & lines] (seq lines)
                 weights (if (included? only exclude label)
                           (update-in weights [label value]
                                      (fnil + 0.) weight)
                           weights)]
             (if (= nlines step)
               ;; After each `step' predict lines, include KL
               ;; into the sequence.
               (cons
                 (let [freqs (normalize-weights weights)]
                   (reduce
                     + (map (fn [label]
                              (KL (true-freqs label) (freqs label)))
                            (keys true-freqs))))
                 (kl-seq* lines 1 weights))
               ;; Otherwise, just accumulate the weights.
               (kl-seq* lines (inc nlines) weights))))))]
    (kl-seq* (predict-seq-skipping skip) 1 {})))

;; REPL command:
(defn total-samples
  "reads results from stdin and returns a map label -> sequence
  of samples, skipping first `skip' predict lines and
  then processing one entry per label each `step' predict lines"
  [& {:keys [skip step only exclude]
      :or {skip 0
           step 1
           only nil
           exclude #{}}}]
  (loop [predicts (predict-seq-skipping skip)
         nlines 1
         samples {}
         seen-labels #{}]
    (if-let [[[label value _] & predicts] (seq predicts)]
      (let [samples (if (and (included? only exclude label)
                             (not (contains? seen-labels label)))
                      (update-in samples [label]
                                 (fnil conj []) value)
                      samples)]
        (if (= nlines step)
          ;; After each nlines forget seen labels
          ;; and start collecting new layer of samples.
          (recur predicts 1
                 samples (empty seen-labels))
          ;; Only a single value for every label is
          ;; collected over each nlines.
          (recur predicts (inc nlines)
                 samples (conj seen-labels label))))
      samples)))

;; REPL command:
(defn ks-seq
  "reads results from stdin and returns a lazy sequence
  of KS distances, skipping first `skip' predict lines and
  then producing a sequence entry each `step' predict lines"
  [true-samples & {:keys [skip step only exclude]
                   :or {skip 0
                        step 1
                        only nil
                        exclude #{}}}]
  (letfn
    [(ks-seq* [lines nlines samples]
       (lazy-seq
       (if (empty? lines) nil
         (let [[[label value _] & lines] (seq lines)
               samples (if (included? only exclude label)
                         (update-in samples [label]
                                    (fnil conj []) value)
                         samples)]
           (if (= nlines step)
             ;; After each `step' predict lines, include KS
             ;; into the sequence.
             (cons (reduce
                     + (map (fn [label]
                              (KS (true-samples label)
                                  (samples label)))
                            (keys true-samples)))
                   (ks-seq* lines 1 samples))
               ;; Otherwise, just collect the samples.
               (ks-seq* lines (inc nlines) samples))))))]
    (ks-seq* (predict-seq-skipping skip) 1 {})))
