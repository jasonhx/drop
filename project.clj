(defproject drop "0.1.0-SNAPSHOT"
  :description "Drop Implementation"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [io.pedestal/pedestal.service "0.5.5"]
                 [io.pedestal/pedestal.immutant "0.5.5"]
                 [metosin/reitit-pedestal "0.2.13"]
                 [metosin/reitit "0.2.13"]
                 [ch.qos.logback/logback-classic "1.2.3" :exclusions
                  [org.slf4j/slf4j-api]]
                 [org.slf4j/jul-to-slf4j "1.7.25"]
                 [org.slf4j/jcl-over-slf4j "1.7.25"]
                 [org.slf4j/log4j-over-slf4j "1.7.25"]
                 [com.taoensso/sente "1.14.0-RC2"]
                 [com.taoensso/timbre "4.10.0"]
                 [org.clojure/clojurescript "1.10.520"]
                 [hiccup "2.0.0-alpha2"]
                 [expound "0.7.2"]]
  :plugins [[lein-cljsbuild "1.1.7"]
            [lein-pprint "1.2.0"]
            [lein-figwheel "0.5.18"]]
  :source-paths ["src/clj" "src/cljc"]
  :profiles {:dev {:cljsbuild
                   {:builds {:client {:figwheel {:on-jsload "simple.core/run"}
                                      :compiler {:main "simple.core"
                                                 :asset-path "js"
                                                 :optimizations :none
                                                 :source-map true
                                                 :source-map-timestamp true}}}}}

             :prod {:cljsbuild
                    {:builds {:client {:compiler {:optimizations :advanced
                                                  :elide-asserts true
                                                  :pretty-print false}}}}}}
  :cljsbuild {:builds {:main
                       {:source-paths ["src/cljs" "src/cljc"]
                        :compiler {:output-to "resources/public/js/client.js"
                                   :optimizations :simple
                                   :pretty-print  true}
                        :jar          true}}}
  :main drop.server
  :aot [drop.server]
  :clean-targets ^{:protect false} ["resources/public/js"]
  :repl-options {:init-ns drop.server})
