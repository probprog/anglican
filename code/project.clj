(defproject embang "0.1.0-SNAPSHOT"
  :description "MAP inference in anglican programs"
  :url "http://bitbucket.org/dtolpin/mappp"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.cli "0.3.1"]
                 [incanter "1.5.5"]]
  :main ^:skip-aot embang.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :source-paths ["ujsrc"]}})
