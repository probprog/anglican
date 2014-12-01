(ns embang.results
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

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

(defn freqs
  "reads the result file in anglican format
  and writes the frequency table for every
  integer-valued predict"
  ([iname] (redir [:in iname] (freqs)))
  ([]
   (loop [lines (line-seq (io/reader *in*))
          weights {}]
     (if (seq lines)
       (let [[line & lines] lines
             line (str/trim line)]
         (if (= (subs line 0 1) ";")
           (recur lines weights) ; comment line
           (let [fields (str/split line #" *, *")
                 label (str/join "," (subvec fields
                                             0 (- (count fields) 2)))
                 [value weight] (map read-string
                                     (subvec fields (- (count fields) 2)))]
             (recur 
               lines
               (if (or (integer? value) (symbol? value))
                 (update-in weights [label value] (fnil + 0.) weight)
                 weights)))))
       (doseq [label (sort (keys weights))]
         (let [total-weight (reduce + (vals (weights label)))]
           (doseq [value (sort (keys (weights label)))]
             (let [weight (/ (double (get-in weights [label value]))
                             total-weight)]
               (printf "%s, %s, %6g, %6g\n"
                       label value weight (Math/log weight))))))))))
