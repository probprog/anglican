(ns embang.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.tools.cli :refer [parse-opts]])
  (:use [embang.inference :only [infer]])
  (:use embang.results))

(defn load-program
  "loads program from clojure module"
  [nsname progname]
  (require (symbol nsname) :reload)
  (var-get (or (ns-resolve (symbol nsname) (symbol progname))
               (throw (Exception. (format "no such program: %s/%s"
                                          nsname progname))))))

(def cli-options
  [;; problems
   ["-a" "--inference-algorithm NAME" "Inference algorithm"
    :default "pgibbs"]

   ["-d" "--debug" "Print debugging information"
    :default false
    :flag true]

   ["-o" "--algorithm-options OPTIONS" "Algorithm options"
    :default []
    :parse-fn (fn [s] (read-string (str "[" s "]")))]

   ["-h" "--help" "print usage summary and exit"]])

(defn usage [summary]
  (str "Usage:
     lein run namespace [program] [option ...]
from the command line, or:
     (-main \"namespace\" [\"program\"] [\"option\" ...])
in the REPL, where `namespace' is the namespace containing the
embedded Anglican program to run, for example:

  bash$ lein run angsrc.branching -a pgibbs \\
            -o \":number-of-sweeps 10 :number-of-particles 50\"

  embang.core=> (-main \"angsrc.branching\" \"-a\" \"pgibbs\"
           \"-o\" \":number-of-sweeps 10 :number-of-particles 50\")
`program' is the first argument of `defanglican'. The namespace
may contain multiple programs. If `program' is omitted, it defaults
to the last component of the namespace (hmm for anglican.hmm,
logi for anglican.logi).

Options:
" summary))

(defn error-msg [errors]
  (str "Error:\n\n"
       (str/join \newline errors)))

(defn -main
  "transforms anglican program to clojure code;
  in REPL run (-main \"--help\") for option summary."
  [& args]
  (let [{:keys [options arguments errors summary] :as parsed-options}
        (parse-opts args cli-options)]

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

       (printf (str ";; Program: %s/%s\n"
                    ";; Inference algorithm: %s\n"
                    ";; Algorithm options: %s\n")
               nsname progname
               (:inference-algorithm options)
               (str/join
                (map (fn [[name value]]
                       (format "\n;;\t%s %s" name value))
                     (partition 2 (:algorithm-options options)))))
       (flush)

       ;; load the algorithm namespace dynamically
       (try
         (require (symbol (format "embang.%s"
                                  (:inference-algorithm options))))

         ;; load the program
         (try
           (let [program (load-program nsname progname)]
             ;; if loaded, run the inference.
             (try
               (apply infer (keyword (:inference-algorithm options))
                      program (:algorithm-options options))
               (catch Exception e
                 (binding [*out* *err*]
                   (printf "Error during inference: %s\n" e))
                 (when (:debug options)
                   (.printStackTrace e)))))

           ;; otherwise, could not load the program
           (catch Exception e
             (binding [*out* *err*]
               (printf "ERROR loading program '%s/%s':\n\t%s\n"
                       nsname progname e)
               (flush)
               (when (:debug options)
                 (.printStackTrace e)))))

         ;; otherwise, could not load the namespace
         (catch Exception e
           (binding [*out* *err*]
             (printf "ERROR loading namespace 'embang.%s':\n\t%s\n"
                     (:inference-algorithm options) e))))))))

(defn -cmd
  "auxiliary commands"
  [& args]
  (assert (= (count args) 1) "Usage: embang.core/-cmd COMMAND")
  (case
    (keyword (first args))
    :freqs (freqs)
    (binding [*out* *err*]
      (println "Unrecognized command: %s" (first args)))))
