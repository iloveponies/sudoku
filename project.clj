(defproject sudoku  "1.0.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [iloveponies.tests/sudoku "0.1.0-SNAPSHOT"]]
  :profiles {:dev {:dependencies [[midje "1.7.0" :exclusions [org.clojure/clojure]]] 
                   :plugins [[lein-midje "3.2"]]}})
