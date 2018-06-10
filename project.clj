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
                 [net.mikera/vectorz-clj "0.44.0"]

                 ;; cljs
                 [net.cgrand/macrovich "0.2.1"]
                 [org.clojure/clojurescript "1.10.238" :scope "provided"]
                 [thinktopic/aljabr "0.1.1" :scope "provided"]]
  :plugins [[codox "0.8.11"]
            [lein-figwheel "0.5.16"]
            [lein-cljsbuild "1.1.7" :exclusions [[org.clojure/clojure]]]]
  :scm {:name "git"
        :url "https://bitbucket.org/probprog/anglican"}
  :repl-options {:timeout 600000}
  :main ^:skip-aot anglican.core
  :target-path "target/%s"
  :deploy-branches ["master" "development"]
  :aliases {"publish" ["do" ["clean"] ["test"] ["uberjar"]]}


  :cljsbuild {:builds
              [{:id "dev"
                :source-paths ["src"]
                :figwheel {:on-jsload "anglican.runtime/on-js-reload"
                           :open-urls ["http://localhost:3449/index.html"]}

                :compiler {:main anglican.bayes-net
                           :asset-path "js/compiled/out"
                           :output-to "resources/public/js/compiled/anglican.js"
                           :output-dir "resources/public/js/compiled/out"
                           :source-map-timestamp true
                           ;; https://github.com/binaryage/cljs-devtools
                           :preloads [devtools.preload]}}

               {:id "min"
                :source-paths ["src"]
                :compiler {:output-to "resources/public/js/compiled/anglican.js"
                           :main anglican.bayes-net
                           :optimizations :advanced
                           :pretty-print false}}]}

  :profiles {:uberjar {:aot :all}

             ;; Setting up nREPL for Figwheel and ClojureScript dev
             ;; Please see:
             ;; https://github.com/bhauman/lein-figwheel/wiki/Using-the-Figwheel-REPL-within-NRepl
             :dev {:dependencies [[binaryage/devtools "0.9.4"]
                                  [figwheel-sidecar "0.5.13"]
                                  [com.cemerick/piggieback "0.2.2"]]
                   ;; need to add dev source path here to get user.clj loaded
                   :source-paths ["src" "dev"]
                   ;; for CIDER
                   ;; :plugins [[cider/cider-nrepl "0.12.0"]]
                   :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
                   ;; need to add the compliled assets to the :clean-targets
                   :clean-targets ^{:protect false} ["resources/public/js/compiled"
                                                     :target-path]}}

  )
