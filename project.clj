(defproject clj-chess-engine "0.1.0.6"
  :description "Chess Engine written in Clojure"
  :url "http://lambda-zone.com"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/math.numeric-tower "0.0.3"]
                 [clojail "1.0.6"]
                 [org.clojure/tools.trace "0.7.5"]
                 [org.clojure/algo.monads "0.1.4"]
                 [org.clojure/core.async "0.1.242.0-44b1e3-alpha"]]
  :main clj-chess-engine.core)
