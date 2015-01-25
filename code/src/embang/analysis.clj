(ns embang.analysis)

;;; Analysis of inference results.

(defn search-sorted 
  "returns a sequence of indices such that if values are
  inserted at the indices into grid, grid remains sorted"
  [grid values]
  (letfn
    [(indices [grid values last-index next-index]
       (lazy-seq 
         (when (seq values)
           (cond
             (empty? grid)
             (if (< last-index next-index) 
               (cons next-index nil)
               nil)

             (> (first grid) (first values))
             (let [more-indices (indices grid (rest values)
                                         next-index next-index)]
               (if (< last-index next-index)
                 (cons next-index more-indices)
                 more-indices))

             :else
             (indices (rest grid) values
                      last-index (inc next-index))))))]
    (indices grid values -1 0)))



