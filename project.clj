(defproject astar "0.1.0-SNAPSHOT"
  :description  "astar"
  :jar-name     "astar.jar"
  :uberjar-name "astar-standalone.jar"
  :main         astar.system
  :min-lein-version "2.7.1"
  :source-paths ["src"]
  
  
  :dependencies [[org.clojure/clojure        "1.9.0-beta4"]
                 [org.clojure/clojurescript  "1.9.946"]
                 [tailrecursion/cljs-priority-map "1.2.1"]
                 [http-kit                   "2.2.0"]
                 [com.stuartsierra/component "0.3.2"]
                 [compojure                  "1.6.0"]
                 [hiccup                     "1.0.5"]
                 [reagent                    "0.7.0"]]

  :plugins      [[lein-figwheel "0.5.14"]
                 [lein-cljsbuild "1.1.7" :exclusions [[org.clojure/clojure]]]]

  :figwheel     {:css-dirs ["resources/public/css"]}
  
  :profiles     {:uberjar {:aot :all
                           :source-paths ["src"]
                           :prep-tasks ["compile" ["cljsbuild" "once" "min"]]}
                 :dev     {:dependencies [[reloaded.repl "0.2.4"]
                                          [expectations "2.2.0-rc3"]
                                          [binaryage/devtools "0.9.4"]
                                          [figwheel-sidecar "0.5.14"]
                                          [com.cemerick/piggieback "0.2.2"]
                                          [org.clojure/data.priority-map "0.0.7"]]
                           :source-paths ["src" "dev"]
                           :repl-options {:nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}
                           :clean-targets ^{ :protect false}["resources/public/js/compiled" :target-path] }}

  :cljsbuild    {:builds [{:id "dev"
                           :source-paths ["src"]
                           :figwheel true
                           :compiler {:main astar.core
                                      :asset-path "js/compiled/out"
                                      :output-to "resources/public/js/compiled/astar.js"
                                      :output-dir "resources/public/js/compiled/out"
                                      :source-map-timestamp true
                                      :preloads [devtools.preload]}}
                          {:id "min"
                           :source-paths ["src"]
                           :compiler {:output-to "resources/public/js/compiled/astar.js"
                                      :main astar.core
                                      :optimizations :advanced
                                      :pretty-print false}}]}
)