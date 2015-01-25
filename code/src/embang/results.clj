(ns embang.results
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.data.json :as json]))

;;;; Helper commands for inference results

;;; Input/output redirection in the REPL.

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

;; Discrete results: total counts.

(defn freqs
  "reads the result file in anglican format
  and writes the frequency table for every
  integer-valued predict"
  ([iname] (redir [:in iname] (freqs)))
  ([]
     (loop [lines (parsed-line-seq (line-seq (io/reader *in*)))
            weights {}]
       (if (seq lines)
         (let [[[label value weight] & lines] lines]
           (recur 
            lines
            (if (or (integer? value) (symbol? value)
                    (contains? #{true false nil} value))
              ;; The value looks like a discrete value.
              (update-in weights [label value] (fnil + 0.) weight)
              weights)))

         (doseq [label (sort-by str (keys weights))]
           (let [total-weight (reduce + (vals (weights label)))]
             (doseq [value (sort (keys (weights label)))]
               (let [weight (/ (double (get-in weights [label value]))
                               total-weight)]
                 (printf "%s, %s, %6g, %6g\n"
                         label value weight (Math/log weight))))))))))


;; Continuous results: mean and standard deviation.

(defn meansd
  "reads the result file in anglican format
  and outputs the mean and standard deviation
  for each predict"
  ([iname] (redir [:in iname] (freqs)))
  ([]
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
             (printf "%s, %6g, %6g\n" label mean sd)))))))
