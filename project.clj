(defproject sudoku "1.0.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [iloveponies.tests/sudoku "0.2.0-SNAPSHOT"]]
  :profiles {:dev {:plugins [[lein-midje "3.2.1"]]}})
