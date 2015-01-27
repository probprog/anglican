(ns embang.results
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.data.json :as json]))

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
      (if (or (integer? value) (symbol? value)
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
          (printf "%s, %s, %6g, %6g\n"
                  label value count (Math/log count)))))))

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

      (doseq [label (sort (keys sums))]
        (let [mean (/ (get-in sums [label :sum])
                      (get-in sums [label :weight]))
              sd (Math/sqrt (-(/ (get-in sums [label :sum2])
                                 (get-in sums [label :weight]))
                                 (* mean mean)))]
          (printf "%s, %6g, %6g\n" label mean sd))))))
