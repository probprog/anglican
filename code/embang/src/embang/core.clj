(ns embang.core
  (:gen-class)
  (:require [clojure.string :as str])
  (:require [clojure.tools.cli :refer [parse-opts]])
  (:use [embang.inference :only [infer]]))
          
(defn load-program
  "loads program from clojure module"
  [nsname progname]
  (require (symbol nsname))
  (var-get (or (ns-resolve (symbol nsname)
                           (symbol progname))
               (throw (Exception. (format "no such program: %s/%s"
                                          nsname progname))))))

(def INFERENCE-ALGORITHMS [:importance :smc :rdb :pgibbs])

(def cli-options
  [;; problems
   ["-a" "--inference-algorithm NAME" "Inference algorithm"
    :default :importance
    :parse-fn keyword
    :validate [(set INFERENCE-ALGORITHMS)
               (str "Must be one of: " (str/join " " INFERENCE-ALGORITHMS))]]

   ["-o" "--algorithm-options OPTIONS" "Algorithm options"
    :default []
    :parse-fn (fn [s] (read-string (str "[" s "]")))]

   ["-h" "--help" "print usage summary and exit"]])

(defn usage [summary]
(str "Usage: lein run namespace [program] [options]

Options:
" summary))

(defn error-msg [errors]
  (str "Error:\n\n"
       (str/join \newline errors)))

(defn -main
  "transforms anglican program to clojure code"
  [& args]
  (let [{:keys [options arguments errors summary] :as parsed-options} (parse-opts args cli-options)]

    ;; Handle help and error conditions.
    (cond
      (:help options) (binding [*out* *err*]
                        (println (usage summary)))
      errors (binding [*out* *err*]
               (println (error-msg errors)))
      :else
      (let [[nsname progname] (if (next arguments) arguments
                                [(first arguments)
                                 (str/replace (first arguments) #".+\." "")])
            program (load-program nsname progname)]
        (apply infer (:inference-algorithm options)
               program (:algorithm-options options))))))
