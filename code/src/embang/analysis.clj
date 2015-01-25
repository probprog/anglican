(ns embang.analysis
  (require [clojure.java.io :as io])
  (use [embang.results :only [parsed-line-seq
                              total-weights
                              normalize-weights]]))

;;;; Analysis of inference results.

;;; Sample distance measures.

(defn KL
  "computes Kullback-Leibler divergence for value frequencies."
  [p-freqs q-freqs] {:pre [(map? p-freqs) (map? q-freqs)]}
  ;; Fix freqs so that have the same keys.
  (let [fix-freqs (fn [a-freqs b-freqs]
                     (reduce (fn [ac k]
                               (if (ac k) ac
                                 (assoc ac k Double/MIN_NORMAL)))
                             a-freqs (keys b-freqs)))
        p-freqs (fix-freqs p-freqs q-freqs)
        q-freqs (fix-freqs q-freqs p-freqs)
        PC (double (reduce + (vals p-freqs)))
        QC (double (reduce + (vals q-freqs)))]
    (reduce (fn [kl k]
              (let [q (/ (q-freqs k) QC)
                    p (/ (p-freqs k) PC)]
                (+ kl (* p (Math/log (/ p q))))))
            0. (keys q-freqs))))

;; For use with KS-two-samples.
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

;; REPL command:
(defn total-freqs
  "reads results from stdin and returns
  a table of total frequences for discrete-valued predicts"
 [& {:keys [exclude] :or {exclude #{}}}]
 (normalize-weights (reduce dissoc (total-weights) exclude)))

;; REPL command:
(defn kl-seq 
  "reads results from stdin and returns a lazy sequence
  of KL distances, skipping first `skip' predict lines and
  then producing a sequence entry each `step' predict lines"
  [true-freqs & {:keys [skip step exclude]
                 :or {skip 0
                      step 1
                      exclude #{}}}]
  (letfn 
    [(kl-seq* [lines iline weights]
       (lazy-seq
         (if (empty? lines) nil
           (let [[[label value weight] & lines] (seq lines)
                 weights (if-not (contains? exclude label)
                           (update-in weights [label value]
                                      (fnil + 0.) weight)
                           weights)]
             (if (= iline step)
               ;; After each `step' predict lines, include KL
               ;; into the sequence. 
               (cons
                 (let [freqs (normalize-weights weights)]
                   (reduce
                     + (map (fn [label]
                              (KL (true-freqs label) (freqs label)))
                            (keys true-freqs))))
                 (kl-seq* lines 0 weights))
               ;; Otherwise, just accumulate the weights.
               (kl-seq* lines (inc iline) weights))))))]
    (kl-seq*
      (drop skip (parsed-line-seq (line-seq (io/reader *in*))))
      0 {})))

;; REPL command:
(defn total-predicts
  "reads results from stdin and returns a lazy sequence of predicts,
  skipping first `skip' predict lines and then producing a sequence
  entry each `step' predict lines"
  [& {:keys [skip step]
      :or {skip 0
           step 1}}]
  (map (fn [[predict]] predict)
    (partition 1 step
               (drop skip
                     (parsed-line-seq (line-seq (io/reader *in*))))))) 

;; REPL command:
(defn ks-seq
  "reads results from stdin and returns a lazy sequence
  of KS distances, skipping first `skip' predict lines and
  then producing a sequence entry each `step' predict lines"
  [true-samples & {:keys [skip step exclude]
                   :or {skip 0
                        step 1
                        exclude #{}}}]
  ;; TODO
  )
