(ns embang.results
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.data.json :as json]
            [clojure.tools.cli :as cli]))

;;;; Helper commands for inference results

;;; Input/output redirection in the REPL.

;; REPL/command-line command:
(defmacro redir
  "Input/output redirection macro,
  useful for running inference and processing results
  from the REPL. The syntax is:

    (redir [:in \"input-file-name\" :out \"output-file-name\"] 
      actions ...)

  Either :in or :out (or both) can be omitted.
  if the output file name begins with '+', '+' is removed
  and the output is appended to the file."
  [[& {:keys [in out] :as args}]  & body]
  (cond
   in 
   (let [rdr (gensym "rdr")]
     `(with-open [~rdr (io/reader ~in)]
        (binding [*in* ~rdr]
          (redir [~@(flatten (seq (dissoc args :in)))]
                 ~@body))))

   out
   (let [wtr (gensym "wtr")]
     `(with-open [~wtr ~(if (= (subs out 0 1) "+")
                          `(io/writer ~(subs out 1) :append true)
                          `(io/writer ~out))]
        (binding [*out* ~wtr]
          (redir [~@(flatten (seq (dissoc args :out)))]
                 ~@body))))

   :else
   `(do ~@body)))

;;; Parsing inference output.

(defmulti parse-line 
  "parses output line and returns [label value height]"
  (fn [line format] format))

(defmethod parse-line :anglican [line format]
  (let [fields (str/split line #" *, *")
        [label value weight] (map edn/read-string
                                  (take-last 3 fields))]
    [label value (or weight 1.)]))

(defmethod parse-line :clojure [line format]
  (edn/read-string line))

(defmethod parse-line :json [line format]
  (json/read-str line))

;; The default method is consistent with the default
;; for print-predict and compatible with original Anglican.

(defmethod parse-line :default [line format]
  (parse-line line :anglican))

(defn parsed-line-seq
  "parses line sequence"
  ([lines] (parsed-line-seq lines nil))
  ([lines format]
     (when-let [[line & lines] (seq lines)]
       (if (re-find #"^\s*;|^\s*$" line)
         ;; meta-info or comment
         (if-let [format-option (re-matches
                                 #"^;;\s*Output format:\s+:(.+)"
                                 line)]
           (recur lines (keyword (format-option 1)))
           (recur lines format))
         (cons (parse-line line format)
               (lazy-seq (parsed-line-seq lines format)))))))

;;; Summary statistics on inference results

;; Discrete results: value frequences (categorical distribution).
;; Helper functions: total-weights and normalize-weights.

(defn total-weights
  "reads results from stdin and returns total weights
  for every value of every discrete-valued predict"
  []
  (reduce
    (fn [weights [label value weight]]
      (if (or (integer? value)
              (symbol? value) (keyword? value)
              (contains? #{true false nil} value))
        ;; The value looks like a discrete value.
        (update-in weights [label value] (fnil + 0.) weight)
        weights))
    {} (parsed-line-seq (line-seq (io/reader *in*)))))

(defn normalize-weights
  "normalizes weights for each label"
  [weights]
  (reduce
    (fn [weights label]
      (let [sum-for-label (reduce + (vals (weights label)))]
        (reduce
          (fn [weights value]
            (update-in weights [label value] / sum-for-label))
          weights (keys (weights label)))))
    weights (keys weights)))

;; REPL/command-line command:
(defn freqs
  "reads results from stdin and writes the frequency table
  for every integer-valued predict"
  []
  (let [total-freqs (normalize-weights (total-weights))]
    (doseq [label (sort-by str (keys total-freqs))]
      (doseq [value (sort (keys (total-freqs label)))]
        (let [count (get-in total-freqs [label value])]
          (println
            (format "%s, %s, %6g, %6g"
                    label value count (Math/log count))))))))

;; Continuous results: mean and standard deviation.

;; REPL/command-line command:
(defn meansd
  "reads results from stdin and writes the mean and
  standard deviation for each predict"
  []
  (loop [lines (parsed-line-seq (line-seq (io/reader *in*)))
         sums {}]
    (if (seq lines)
      (let [[[label value weight] & lines] lines]
        (recur 
          lines
          (if (number? value)
            ;; The value is a numeric value for which mean
            ;; and standard deviation can be computed.
            (let [weighted-value (* value weight)]
              (-> sums
                  (update-in [label :weight] (fnil + 0.) weight)
                  (update-in [label :sum] (fnil + 0.) weighted-value)
                  (update-in [label :sum2] (fnil + 0.) 
                             (* weighted-value weighted-value))))
            sums)))

      (doseq [label (sort-by str (keys sums))]
        (let [mean (/ (get-in sums [label :sum])
                      (get-in sums [label :weight]))
              sd (Math/sqrt (-(/ (get-in sums [label :sum2])
                                 (get-in sums [label :weight]))
                                 (* mean mean)))]
          (println (format "%s, %6g, %6g" label mean sd)))))))

;;; Sample distance measures

(defn KL
  "computes Kullback-Leibler divergence for value frequencies."
  [p-freqs q-freqs] {:pre [(map? p-freqs) (map? q-freqs)]}
  ;; Fix freqs so that p and q have the same keys.
  (reduce (fn [kl k]
            (let [q (q-freqs k)
                  p (p-freqs k)]
              (if (and p q)
                (+ kl (* p (Math/log (/ p q))))
                kl)))
          0. (keys q-freqs)))

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
  (and (or (empty? only) (contains? only label))
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

;;; Diff: difference between prediction and truth

;; Command-line utility that takes the configuration and outputs of
;; the distance for a single experiment outcome and the truth.
;; 
;; The truth is stored in an external file in predict format. So,
;; the configuration would look like

#_ (:period 19                          ; predicts per particle
    :only [(get-state 1) (get-state 2)] ; predicts to account for
    :exclude []                         ; predicts to ignore
    :distance :ks                       ; type of distance
    :truth "hhmm.truth")                ; resource with the truth

;; Two additional parameters, skip and step, 
;; are provided on the command line.

(def cli-options
  [;; problems
   ["-s" "--skip N" "Skip first N predict lines"
    :default 0
    :parse-fn #(Integer/parseInt %)]
   ["-t" "--step N" "Output distance each N predict lines"
    :default 1
    :parse-fn #(Integer/parseInt %)]
   ["-h" "--help" "print usage summary and exit"]])

(defn usage [summary]
  (str "Usage:
     lein run conf.edn [option ...] < results > distances

Options:
" summary))

(defn error-msg [errors]
  (str/join "\n\t" (cons "ERROR parsing the command line:" errors)))

(defn diff
  "command-line/REPL utility that takes problem configuration 
  and inference output and produces differences between
  the input and the truth"
  [& args]
  (let [{:keys [options arguments errors summary] :as parsed-options}
        (cli/parse-opts args cli-options)]

    ;; Handle help and error conditions.
    (cond
      (:help options) (binding [*out* *err*]
                        (println (usage summary)))

      errors (binding [*out* *err*]
               (println (error-msg errors)))

      (empty? arguments) (binding [*out* *err*]
                           (println (usage summary)))

      :else
      (let [config (apply hash-map 
                          (with-open [in (java.io.PushbackReader.
                                           (io/reader
                                             (io/resource
                                               (first arguments))))]
                            (edn/read in)))]
        (binding [*out* *err*]
          (doseq [[option value] (sort-by first
                                          (merge options config))]
            (println (format ";; %s %s" option value))))
        (let [[mk-seq get-truth] (case (:distance config)
                                   :kl [kl-seq total-freqs]
                                   :ks [ks-seq total-samples])
              truth (redir [:in (io/resource (:truth config))]
                      (get-truth))
              period (or (:period config) 1)]
          (doseq [distance (mk-seq truth
                                   :skip (* (:skip options) period)
                                   :step (* (:step options) period)
                                   :only (set (:only config))
                                   :exclude (set (:exclude config)))]
            (prn distance)))))))
