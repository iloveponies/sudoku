(defproject sudoku "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.3.0"]]
  :repositories {"stuart" "http://stuartsierra.com/maven2"}
  :plugins [[lein-midje "2.0.0-SNAPSHOT"]]
  :profiles
    {:dev
     {:dependencies [[midje "1.4.0"]
                     [com.stuartsierra/lazytest "1.2.3"]]}})
