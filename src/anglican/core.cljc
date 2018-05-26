(ns anglican.core
  "REPL and command line"
  #?(:clj (:gen-class))
  (:refer-clojure :exclude [rand rand-int rand-nth read-string])
  (:require #?(:clj [clojure.edn :refer [read-string]
              :cljs [cljs.reader :refer [read-string]]])
            [clojure.string :as str]
            [clojure.tools.cli :as cli]
            [anglican.inference :refer [infer 
                                        warmup stripdown drop-invalid
                                        print-predicts]]
            #?(:clj [anglican.results :refer [redir freqs meansd diff]])))

#?(:clj
  (defn load-algorithm
    "loads algorithm by requiring the namespace"
    [algorithm]
    (let [algorithm-namespace (symbol
                               (format "anglican.%s" (name algorithm)))]
      (try (require algorithm-namespace) true
           (catch Exception e
             (binding [*out* *err*]
               (println
                (format "ERROR loading namespace '%s':\n\t%s"
                        algorithm-namespace e)))
             false)))))

#?(:clj
  (defn load-program
    "loads program from clojure module"
    [nsname progname]
    (require (symbol nsname) :reload)
    (var-get (or (ns-resolve (symbol nsname) (symbol progname))
                 (throw (Exception. (format "no such program: %s/%s"
                                            nsname progname)))))))

#?(:clj
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

     ["-f" "--output-format FORMAT" "Output format"
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

     ["-v" "--value V" "Initial value to pass to the program"
      :default nil
      :parse-fn read-string]

     ["-w" "--warmup FLAG" "Pre-evaluate the program"
      :default true
      :parse-fn read-string
      :validate [#(contains? #{true false} %) "must be boolean"]]

     ["-h" "--help" "Print usage summary and exit"]]))

#?(:clj
  (defn usage [summary]
    (str "Usage:
    lein run namespace [program] [option ...]

from the command line, or:
    (m! namespace [program] [\"option\" ...])

in the REPL, where `namespace' is the namespace containing the
embedded Anglican program to run. For example, if the namespace
is called `helloworld` then the program in this namespace can be
run using

    bash$ lein run helloworld -a pgibbs -n 500 \\
               -o \":number-of-particles 50\"

    anglican.core=> (m! -a pgibbs -n 500 -o \":number-of-particles 50\"
                      helloworld)

`program' is the first argument of `defquery'. The namespace
may contain multiple programs. If `program' is omitted, it defaults
to the last component of the namespace (hmm for anglican.hmm,
logi for anglican.logi).

Options:
" summary)))

(defn error-msg [errors]
  (str/join "\n\t" (cons "ERROR parsing the command line:" errors)))

#?(:clj
  (defn main
    "runs the interface and the auxiliary commands"
    [& args]
    (if (= (ffirst args) \:)
      ;; Run auxiliary commands
      (case (read-string (first args))
        :diff (apply diff (rest args))
        :freqs (freqs)
        :meansd (meansd)
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
             (format (str ";; Program: %s/%s %s\n"
                          ";; Inference algorithm: %s %s\n"
                          ";; Number of samples: %s (*%s+%s)\n"
                          ";; Output format: %s")
                     nsname progname (if (some? (:value options))
                                       (:value options)
                                       "")
                     (:inference-algorithm options)
                     `(:warmup ~(:warmup options)
                               ~@(:algorithm-options options))
                     (:number-of-samples options)
                     (:thin options) (:burn options)
                     (:output-format options)))

            ;; Load the program.
            (try
              (let [program (load-program nsname progname)
                    value (:value options)
                    [program value] (if (:warmup options) 
                                      [(warmup program value) nil]
                                      [program value])]

                ;; If loaded, run the inference.
                (try
                  (loop [i 0
                         states (as->
                                    (apply infer
                                           (:inference-algorithm options)
                                           program value
                                           (:algorithm-options options))
                                    states
                                  ;; Burn samples.
                                  (drop (:burn options) states)
                                  ;; Thin samples.
                                  (take-nth (:thin options) states))]
                    (when-not (= i (:number-of-samples options))
                      (when (seq states)
                        (let [state (first states)]
                          (print-predicts (:output-format options) state)
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
                    (.printStackTrace e)))))))))))

#?(:clj
  (defmacro m!
    "invoking main from the REPL"
    [& args]
    ` (main ~@(map str args))))

#?(:clj
  (defn -main
    "invoking main from the command line"
    [& args]
    (apply main args)
    (shutdown-agents)))

;; Rich REPL
;;
;; In an alternative paradigm of interaction, results are
;; manipulated in the REPL (Leiningen, Gorilla). `doquery'
;; accepts the query as a callable object and returns a
;; lazy sequence of states.

(defn doquery
  "performs inference query;
  returns lazy sequence of states"
  [algorithm query value & options]
  (do
    ;; Automatically load the inference algorithm.
    #?(:clj (load-algorithm algorithm))

    (let [options* (apply hash-map options)]
      ;; Optionally, warm up the query by pre-evaluating
      ;; the determenistic prefix.
      (let [[query value] (if (:warmup options* true)
                            [(warmup query value) nil]
                            [query value])

            ;; Call the inference to create
            ;; a lazy sequence of states.
            states (apply infer algorithm query value options)

            ;; Some program states can have a zero weight.
            ;; Filter out these states by default.
            valid-states (if (:drop-invalid options* true)
                           (drop-invalid states)
                           states)]

        ;; A state may contain private algorithm-specific entries.
        ;; Strip them down for cleaner output.
        (if (:stripdown options* true)
          (map stripdown valid-states)
          valid-states)))))
