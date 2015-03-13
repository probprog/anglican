(defproject embang "0.4.9"
  :description "Anglican, a probabilistic programming system"
  :url "http://bitbucket.org/dtolpin/embang"
  :license {:name "GNU General Public License Version 3"
            :url "http://www.gnu.org/licenses/gpl.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/tools.cli "0.3.1"]
                 [org.clojure/data.priority-map "0.0.5"]
                 [org.clojure/data.json "0.2.5"]
                 [com.taoensso/timbre "3.3.1"]
                 [colt "1.2.0"]
                 [org.clojure/math.combinatorics "0.0.8"]
                 [net.mikera/core.matrix "0.32.1"]
                 [net.mikera/vectorz-clj "0.28.0"]]
  :scm {:name "git"
        :url "https://bitbucket.org/dtolpin/embang"}
  :repl-options {:timeout 600000}
  :jvm-opts ["-Xmx1024m" "-Xss1024k"]
  :main ^:skip-aot embang.core
  :target-path "target/%s"
  :java-source-paths ["src"]
  :resource-paths ["../examples"]
  :profiles {:uberjar {:aot :all}})
