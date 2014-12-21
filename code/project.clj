(defproject embang "0.1.0-SNAPSHOT"
  :description "MAP inference in anglican programs"
  :url "http://bitbucket.org/dtolpin/mappp"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.cli "0.3.1"]
                 [org.clojure/data.json "0.2.5"]
                 [colt "1.2.0"]
                 [org.clojure/math.combinatorics "0.0.8"]
                 [net.mikera/core.matrix "0.31.1"]
                 [net.mikera/vectorz-clj "0.26.2"]]
  :main ^:skip-aot embang.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
