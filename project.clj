(defproject sudoku "1.0.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [iloveponies.tests/sudoku "0.1.0-SNAPSHOT"]
                 [org.clojure/tools.trace "0.7.9"]
                 ]
  :profiles {:dev {:plugins [[lein-midje "3.1.1"]]}})
