(ns embang.analysis)

;;; Analysis of inference results.

;; For use with KS-two-samples.
(defn search-sorted 
  "returns a sequence of indices such that if values are
  inserted at the indices into grid, grid remains sorted;
  assumes that both grid and values are sorted"
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

(defn KS-two-samples
  "computes Kolmogorov-Smirnov distance for two samples"
  [sa sb]
  (let [sa (sort sa)
        sb (sort sb)
        s (sort (concat sa sb))
        cdf (fn [sx]
              (let [nx (double (count sx))]
                (map #(/ (double %) nx) (search-sorted sx s))))]
    (reduce max (map #(Math/abs (- %1 %2)) (cdf sa) (cdf sb)))))
  
(defn KL
  "computes Kullback-Leibler divergence for counts"
  [p-counts q-counts] {:pre [(map? p-counts) (map? q-counts)]}
  ;; Fix counts so that have the same keys.
  (let [fix-counts (fn [a-counts b-counts]
                     (reduce (fn [ac k]
                               (if (ac k) ac
                                 (assoc ac k Double/MIN_NORMAL)))
                             a-counts (keys b-counts)))
        p-counts (fix-counts p-counts q-counts)
        q-counts (fix-counts q-counts p-counts)
        PC (double (reduce + (vals p-counts)))
        QC (double (reduce + (vals q-counts)))]
    (prn p-counts q-counts)
    (reduce (fn [kl k]
              (let [q (/ (q-counts k) QC)
                    p (/ (p-counts k) PC)]
                (+ kl (* p (Math/log (/ p q))))))
            0. (keys q-counts))))
