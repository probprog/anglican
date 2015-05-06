(defproject anglican "0.6.6-SNAPSHOT"
  :description "Anglican, a probabilistic programming system"
  :url "http://bitbucket.org/dtolpin/anglican"
  :license {:name "GNU General Public License Version 3"
            :url "http://www.gnu.org/licenses/gpl.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.cli "0.3.1"]
                 [org.clojure/data.json "0.2.6"]
                 [com.taoensso/timbre "3.4.0"]
                 [colt "1.2.0"]
                 [net.mikera/core.matrix "0.33.2"]
                 [net.mikera/core.matrix.stats "0.5.0"]
                 [net.mikera/vectorz-clj "0.29.0"]]
  :plugins [[codox "0.8.11"]]
  :scm {:name "git"
        :url "https://bitbucket.org/dtolpin/anglican"}
  :repl-options {:timeout 600000}
  :jvm-opts ["-Xmx1024m" "-Xss1024k"]
  :javac-options ["-target" "1.6" "-source" "1.6"]
  :main ^:skip-aot anglican.core
  :target-path "target/%s"
  :java-source-paths ["src"]
  :resource-paths ["../examples"]
  :profiles {:uberjar {:aot :all}}
  :deploy-branches ["master"]
  :aliases {"publish" ["do" ["clean"] ["test"] ["uberjar"]]})
