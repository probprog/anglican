(ns embang.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.tools.cli :refer [parse-opts]])
  (:use [embang.inference :only [infer]]))

(defn load-program
  "loads program from clojure module"
  [nsname progname]
  (require (symbol nsname))
  (var-get (or (ns-resolve (symbol nsname)
                           (symbol progname))
               (throw (Exception. (format "no such program: %s/%s"
                                          nsname progname))))))

(def cli-options
  [;; problems
   ["-a" "--inference-algorithm NAME" "Inference algorithm"
    :default :importance
    :parse-fn keyword]

   ["-d" "--debug" "Print debugging information"
    :default false
    :flag true]

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
  (let [{:keys [options arguments errors summary] :as parsed-options}
        (parse-opts args cli-options)]

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
            algorithm (:inference-algorithm options)
            options (:algorithm-options options)]

        (binding [*out* *err*]
          (printf "Inference algorithm: '%s'\nAlgorithm options: %s\nProgram: %s/%s\n"
                  algorithm options nsname progname) (flush))

        ;; load the algorithm namespace dynamically
        (try
          (require (symbol (format "embang.%s" (name algorithm))))

          ;; load the program
          (try
            (let [program (load-program nsname progname)]
              ;; if loaded, run the inference.
              (try
                (apply infer algorithm program options)
                (catch Exception e
                  (binding [*out* *err*]
                    (printf "Error during inference: %s" e))
                  (when (:debug options)
                    (.printStackTrace e)))))

            ;; otherwise, could not load the program
            (catch Exception e
              (binding [*out* *err*]
                (printf "Cannot load program '%s/%s': %s"  nsname progname e))))

          ;; otherwise, could not load the namespace
          (catch Exception e
            (binding [*out* *err*]
              (printf
                "Cannot load namespace 'embang.%s' for inference algorithm '%s':\n\t%s"
                (name algorithm) (name algorithm) e))))))))
