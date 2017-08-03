(defproject anglican "1.1.0-SNAPSHOT"
  :description "Anglican, a probabilistic programming system"
  :url "http://bitbucket.org/probprog/anglican"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.cli "0.3.5"]
                 [org.clojure/data.json "0.2.6"]
                 [com.climate/claypoole "1.1.2"]
                 [com.taoensso/timbre "4.3.1"]
                 [org.apache.commons/commons-math3 "3.6.1"]
                 [net.mikera/core.matrix "0.52.0"]
                 [net.mikera/core.matrix.stats "0.7.0"]
                 [net.mikera/vectorz-clj "0.44.0"]]
  :plugins [[codox "0.8.11"]]
  :scm {:name "git"
        :url "https://bitbucket.org/probprog/anglican"}
  :repl-options {:timeout 600000}
  :main ^:skip-aot anglican.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :deploy-branches ["master" "development"]
  :aliases {"publish" ["do" ["clean"] ["test"] ["uberjar"]]})
