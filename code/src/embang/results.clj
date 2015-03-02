(ns embang.results
  (:refer-clojure :exclude [read read-string])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.edn :refer [read read-string]]
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
  (fn [format line] format))

(defmethod parse-line :anglican [_ line]
  (let [fields (str/split line #" *, *")
        [label value log-weight] (map read-string
                                      (take-last 3 fields))]
    [label value (or log-weight 0.)]))

(defmethod parse-line :clojure [_ line]
  (read-string line))

(defmethod parse-line :json [_ line]
  (json/read-str line))

;; The default method is consistent with the default
;; for print-predict and compatible with original Anglican.

(defmethod parse-line :default [_ line]
  (parse-line :anglican line))

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
         (cons (parse-line format line)
               (lazy-seq (parsed-line-seq lines format)))))))

;;; Summary statistics on inference results

;; Discrete results: value frequences (categorical distribution).

(defn countable?
  "true when the value looks countable"
  [value]
  (every?
    #(or (integer? %)
         (symbol? %) (keyword? %)
         (contains? #{true false nil} %))
    (flatten (list value))))

(defn weights-to-freqs
  "computes frequences from weights for each label"
  [weights]
  (reduce
    (fn [weights [label label-weights]]
      (let [sum-for-label (reduce + (vals label-weights))]
        (reduce
          (fn [weights value]
            (update-in weights [label value] / sum-for-label))
          weights (keys label-weights))))
    weights (seq weights)))

(defn fq-seq
  "accepts a sequence of predicts
  and produces a sequence of frequencies"
  [predicts]
  (letfn
    [(fq-seq* [predicts weights]
       (lazy-seq
         (when-let [[[label value log-weight] & predicts]
                    (seq predicts)]
           (if (and (countable? value) (number? log-weight))
             (let [weights (update-in
                             weights [label value]
                             (fnil + 0.) (Math/exp log-weight))]
               (cons (weights-to-freqs weights)
                     (fq-seq* predicts weights)))
             (fq-seq* predicts weights)))))]
    (fq-seq* predicts {})))

;; Continuous results: mean and standard deviation.

(defn sums-to-meansd
  "accepts total sums for labels
  and computes mean and sd for each label"
  [sums]
  (reduce (fn [meansd [label {:keys [sum sum2 weight]}]]
            (let [mean (/ sum weight)
                  sd (Math/sqrt (- (/ sum2 weight) (* mean mean)))]
              (assoc meansd label {:mean mean :sd sd})))
          {} (seq sums)))

(defn ms-seq
  "accepts a sequence of predicts
  and produces a sequence of mean and sd for each label"
  [predicts]
  (letfn
    [(ms-seq* [predicts sums]
       (lazy-seq
         (when-let [[[label value log-weight] & predicts]
                    (seq predicts)]
           (if (and (number? value) (number? log-weight))
            (let [weight (Math/exp log-weight)
                  weighted-value (* value weight)
                  sums (-> sums
                           (update-in [label :weight] (fnil + 0.)
                                      weight)
                           (update-in [label :sum] (fnil + 0.)
                                      weighted-value)
                           (update-in [label :sum2] (fnil + 0.)
                                      (* value weighted-value)))]
              (cons (sums-to-meansd sums)
                    (ms-seq* predicts sums)))
             (ms-seq* predicts sums)))))]
    (ms-seq* predicts {})))

(defn sm-seq
  "accets a sequence of predicts and produces sample sequence"
  ;; Ignores weights.
  [predicts]
  (letfn
    [(sm-seq* [predicts sseqs]
       (lazy-seq
         (when-let [[[label value log-weight] & predicts]
                    (seq predicts)]
           (if (number? log-weight)
             (let [sseqs (update-in sseqs [label] (fnil conj []) value)]
               (cons sseqs (sm-seq* predicts sseqs)))
             (sm-seq* predicts sseqs)))))]
    (sm-seq* predicts {})))

;;; Filtering labels

(defn included?
  "true when the label should be included"
  [only exclude label]
  (and (or (empty? only) (contains? only label))
       (not (contains? exclude label))))

;;; Total results

(defn totals
  "reads results from stdin and returns totals filtered by smry-seq"
  [smry-seq & {:keys [only exclude]
               :or {only nil
                    exclude #{}}}]
  (let [totals (-> (io/reader *in*)
                   line-seq
                   parsed-line-seq
                   smry-seq
                   last)]
    (select-keys totals
                 (filter #(included? only exclude %)
                         (keys totals)))))

;;;; Sample distance metrics

;;; Metric formulas

(defn KL
  "computes Kullback-Leibler divergence for value frequencies."
  [p-freqs q-freqs] {:pre [(map? p-freqs) (map? q-freqs)]}
  (reduce (fn [kl k]
            (let [q (q-freqs k)
                  p (p-freqs k)]
              (if (and p q)
                (+ kl (* p (Math/log (/ p q))))
                kl)))
          0. (keys q-freqs)))

(defn L2
  "computes L2 distance for value frequencies."
  [p-freqs q-freqs] {:pre [(map? p-freqs) (map? q-freqs)]}
  (Math/sqrt
    (reduce (fn [l2 k]
              (let [d (- (p-freqs k 0.) (q-freqs k 0.))]
                (+ l2 (* d d))))
            0. (keys (merge p-freqs q-freqs)))))

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

;;; Multimethods dispatching on metrics

(defmulti get-truth
  "reads truth from stdin and returns
  a structure suitable for diff-seq"
  (fn [distance-type & options] distance-type))

;; Not all metrics use ground truth.
(defmethod get-truth :default [_ & _] nil)

(defmulti diff-seq
  "reads results from stdin and returns a lazy sequence
  of distances from the truth"
  (fn [distance-type truth & options] distance-type))

;;; Filtering sequences

;; Filtering options are forwarded. Clojure seems to have
;; an inconsistency in that a function can't be applied
;; to its own optional arguments. Fix it.

(defn unmap
  "transforms keyword arguments from map to sequence"
  [options]
  (if (map? options)
    (apply concat options)
    options))

(defn read-summaries
  "reads summaries from standard input"
  [smry-seq & {:keys [skip step only exclude]
               :or {skip 0 
                    step 1
                    only nil
                    exclude #{}} :as options}]
  (->> (io/reader *in*)
       line-seq
       parsed-line-seq
       (drop skip)
       smry-seq
       (take-nth step)
       rest
       (map (fn [summary] 
              (select-keys summary 
                           (filter #(included? only exclude %)
                                   (keys summary)))))))

(defn distance-seq
  "reads results from stdin and returns a lazy sequence
  of distances, skipping first `skip' predict lines and
  then producing a sequence entry each `step' predict lines"
  [smry-seq distance truth & {:keys [skip step only exclude]
                              :or {skip 0
                                   step 1
                                   only nil
                                   exclude #{}} :as options}]
  (map (fn [summary]
         (reduce + (map #(distance (truth %) (summary %))
                        (keys summary))))
       (apply read-summaries smry-seq (unmap options))))

;;; Discrete predicts

(defmethod diff-seq :fq
  [_ _ & options]
  (read-summaries fq-seq (unmap options)))

(defmethod get-truth :kl [_ & options]
  (apply totals fq-seq options (unmap options)))

(defmethod diff-seq :kl
  [_ truth & options]
  (apply distance-seq fq-seq KL truth (unmap options)))

(defmethod get-truth :l2 [_ & options]
  (apply totals fq-seq (unmap options)))

(defmethod diff-seq :l2
  [_ truth & options]
  (apply distance-seq fq-seq L2 truth (unmap options)))

;;; Continuous predicts

(defmethod diff-seq :ms
  [_ _ & {:keys [skip step only exclude]
          :or {skip 0
               step 1
               only nil
               exclude #{}}:as options}]
  (apply read-summaries ms-seq (unmap options)))

(defmethod get-truth :ks [_ & options]
  (apply totals sm-seq options))

(defmethod diff-seq :ks
  [_ truth & options]
  (prn options)
  (apply distance-seq sm-seq KS truth (unmap options)))

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

(def default-config
     "default option values"
     {:distance :kl
	  :period 1
	  :burn 0
	  :thin 1})

(def cli-options
  [["-b" "--burn N" "Skip first N predict lines"
    :parse-fn #(Integer/parseInt %)]
   ["-c" "--config CONFIG" "config resource"
    :default nil]
   ["-d" "--distance d" "distance type"
    :parse-fn keyword
    :validate [#{:fq :ms :kl :l2 :ks} "unrecognized distance"]]
   ["-e" "--exclude LABELS" "predicts to exclude from statistics"
    :default #{}
    :parse-fn (fn [s] (read-string (str "#{" s "}")))]
   ["-o" "--only LABELS" "predicts to keep in statistics"
    :default nil
    :parse-fn (fn [s] (read-string (str "#{" s "}")))]
   ["-p" "--period N" "number of predicts per sample"
    :parse-fn #(Integer/parseInt %)]
   ["-t" "--thin N" "Output distance each N predict lines"
    :parse-fn #(Integer/parseInt %)]
   ["-T" "--truth resource" "Resource containing ground truth"]
   ["-h" "--help" "print usage summary and exit"]])

(defn usage [summary]
  (str "Usage:
     lein run :diff conf.edn [option ...] < results > distances

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

      (seq arguments) (binding [*out* *err*]
                        (println (usage summary)))

      :else
      (let [config (when (:config options)
                     (apply hash-map
                            (with-open [in (java.io.PushbackReader.
                                             (io/reader
                                               (io/resource
                                                 (:config options))))]
                              (read in))))
            options (merge default-config config options)]
        (binding [*out* *err*]
          (doseq [[option value] (sort-by first options)]
            (println (format ";; %s %s" option value))))
        (let [truth
              (when (:truth options)
                (redir [:in (io/resource (:truth options))]
                       (get-truth (:distance options)
                                  :only (set (:only options))
                                  :exclude (set (:exclude options)))))
              period (or (:period options) 1)]
          (doseq [distance
                  (diff-seq (:distance options) truth
                            :skip (* (:burn options) period)
                            :step (* (:thin options) period)
                            :only (set (:only options))
                            :exclude (set (:exclude options)))]
            (pprint distance)))))))

(letfn [(to-comparable
          [value]
          (if (instance? java.lang.Comparable value)
            value
            (str value)))]

  ;; REPL command
  (defn freqs
    "reads results from stdin and writes the frequency table
    for every integer-valued predict"
    []
    (let [total-freqs (totals fq-seq)]
      (doseq [label (sort-by to-comparable (keys total-freqs))]
        (doseq [value (sort-by to-comparable
                               (keys (total-freqs label)))]
          (let [weight (get-in total-freqs [label value])]
            (println
              (format "%s, %s, %6g, %6g"
                      label value weight (Math/log weight))))))))

  ;; REPL command
  (defn meansd
    "reads results from stdin and writes the mean and
    standard deviation for each predict"
    []
    (let [total-meansd (totals ms-seq)]
      (doseq [label (sort-by to-comparable (keys total-meansd))]
        (let [{:keys [mean sd]} (get total-meansd label)]
          (println (format "%s, %6g, %6g" label mean sd)))))))
