(ns embang.core (:gen-class)
  (:refer-clojure :exclude [rand rand-int rand-nth read-string])
  (:require [clojure.edn :refer [read-string]]
            [clojure.string :as str]
            [clojure.tools.cli :as cli])
  (:use [embang.inference :only [warmup infer print-predicts]])
  (:use [embang.results :only [redir freqs meansd diff]]))

(defn load-algorithm
  "loads algorithm by requiring the namespace"
  [algorithm]
  (let [algorithm-namespace (symbol
                              (format "embang.%s" (name algorithm)))]
    (try (require algorithm-namespace) true
         (catch Exception e
           (binding [*out* *err*]
             (println
               (format "ERROR loading namespace '%s':\n\t%s"
                       algorithm-namespace e)))
           false))))

(defn load-program
  "loads program from clojure module"
  [nsname progname]
  (require (symbol nsname) :reload)
  (var-get (or (ns-resolve (symbol nsname) (symbol progname))
               (throw (Exception. (format "no such program: %s/%s"
                                          nsname progname))))))

(def cli-options
  [["-a" "--inference-algorithm NAME" "Inference algorithm"
    :default (#(do (load-algorithm %) %) :lmh)
    :parse-fn keyword
    :validate [load-algorithm "unrecognized algorithm name."]]

   ["-b" "--burn N" "Skip first N samples"
    :default 0
    :parse-fn #(Integer/parseInt %)]

   ["-d" "--debug" "Print debugging information"
    :default false
    :flag true]

   ["-f" "--output-format FORMAT" "output format"
    :default :anglican
    :parse-fn keyword
    :validate [#{:anglican :clojure :json}
               "must be one of anglican, clojure, json."]]

   ["-n" "--number-of-samples N" "Output predicts for N samples"
    :default nil
    :parse-fn #(Integer/parseInt %)]

   ["-o" "--algorithm-options OPTIONS" "Algorithm options"
    :default []
    :parse-fn (fn [s] (read-string (str "[" s "]")))]

   ["-t" "--thin N" "Retain each Nth sample"
    :default 1
    :parse-fn #(Integer/parseInt %)]

   ["-h" "--help" "print usage summary and exit"]])

(defn usage [summary]
  (str "Usage:
    lein run namespace [program] [option ...]

from the command line, or:
    (m! namespace [program] [\"option\" ...])

in the REPL, where `namespace' is the namespace containing the
embedded Anglican program to run, for example:

    bash$ lein run angsrc.branching -a gibbs -n 500 \\
               -o \":number-of-particles 50\"

    embang.core=> (m! -a gibbs -n 500 -o \":number-of-particles 50\"
                      angsrc.branching)

`program' is the first argument of `defanglican'. The namespace
may contain multiple programs. If `program' is omitted, it defaults
to the last component of the namespace (hmm for anglican.hmm,
logi for anglican.logi).

Options:
" summary))

(defn error-msg [errors]
  (str/join "\n\t" (cons "ERROR parsing the command line:" errors)))

(defn main
  "runs the interface and the auxiliary commands"
  [& args]
  (if (= (ffirst args) \:)
    ;; Run auxiliary commands
    (case (read-string (first args))
      :freqs (freqs)
      :meansd (meansd)
      :diff (apply diff (rest args))
      (binding [*out* *err*]
        (println (format "Unrecognized command: %s" (first args)))))

    ;; Run the inference
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
        (let [[nsname progname] (if (next arguments) arguments
                                  [(first arguments)
                                   (str/replace (first arguments)
                                                #".+\." "")])
              inference-algorithm (:inference-algorithm options)
              algorithm-options (:algorithm-options options)]

          (println
            (format (str ";; Program: %s/%s\n"
                         ";; Inference algorithm: %s\n"
                         ";; Number of samples: %s (*%s+%s) \n"
                         ";; Output format: %s\n"
                         ";; Algorithm options: %s")
                    nsname progname
                    (:inference-algorithm options)
                    (:number-of-samples options)
                    (:thin options)
                    (:burn options)
                    (:output-format options)
                    (str/join
                      (map (fn [[name value]]
                             (format "\n;;\t%s %s" name value))
                           (partition 2 (:algorithm-options options))))))

          ;; Load the program.
          (try
            (let [program (load-program nsname progname)]

              ;; If loaded, run the inference.
              (try
                (loop [i 0
                       states (as->
                                (apply infer
                                       (:inference-algorithm options)
                                       (warmup program)
                                       (:algorithm-options options))
                                states
                                ;; Burn samples.
                                (drop (:burn options) states)
                                ;; Thin samples.
                                (map first
                                     (partition
                                       1 (:thin options) states)))]
                  (when-not (= i (:number-of-samples options))
                    (when (seq states)
                      (let [state (first states)]
                        (print-predicts state
                                        (:output-format options))
                        (recur (inc i) (rest states))))))
                (catch Exception e
                  (binding [*out* *err*]
                    (println (format "Error during inference: %s" e)))
                  (when (:debug options)
                    (.printStackTrace e)))))

            ;; Otherwise, could not load the program.
            (catch Exception e
              (binding [*out* *err*]
                (println
                  (format "ERROR loading program '%s/%s':\n\t%s"
                          nsname progname e))
                (when (:debug options)
                  (.printStackTrace e))))))))))

(defmacro m!
  "invoking main from the REPL"
  [& args]
  ` (main ~@(map str args)))

(defn -main
  "invoking main from the command line"
  [& args]
  (apply main args)
  (shutdown-agents))
