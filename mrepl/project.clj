(defproject mrepl "0.2.2-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [embang "0.6.2"]]
  :plugins [[lein-gorilla "0.3.4"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
