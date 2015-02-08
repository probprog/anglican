(defproject embang "0.3.1-SNAPSHOT"
  :description "MAP inference in anglican programs"
  :url "http://bitbucket.org/dtolpin/mappp"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.cli "0.3.1"]
                 [org.clojure/data.priority-map "0.0.5"]
                 [org.clojure/data.json "0.2.5"]
                 [com.taoensso/timbre "3.3.1"]
                 [colt "1.2.0"]
                 [org.clojure/math.combinatorics "0.0.8"]
                 [net.mikera/core.matrix "0.32.1"]
                 [net.mikera/vectorz-clj "0.28.0"]]
  :main ^:skip-aot embang.core
  :target-path "target/%s"
  :java-source-paths ["src"]
  :resource-paths ["../examples"]
  :profiles {:uberjar { ;; Prevent compilation of Anglican source code,
                        ;; generation class names are too long for 
                        ;; file names.
                       :aot [#"embang.*"]}})
