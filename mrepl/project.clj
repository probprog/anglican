(defproject mrepl "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [embang "0.5.1"]]
  :plugins [[lein-gorilla "0.3.4"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
