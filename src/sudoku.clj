(ns sudoku
  (:require [clojure.set :as set])
  (:require [clojure.math.combinatorics :as combo])
  )

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not ( = 0 (value-at board coord))))

(defn row-values [board [row col]]
  (set (get-in board [row])))

(defn col-values [board [row col]]
  (set (map #(get-in board [% col]) (range 0 9))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board [x y]]
  (let [startx (* 3 (quot x 3))
        starty (* 3 (quot y 3))
        rangex (range startx (+ startx 3))
        rangey (range starty (+ starty 3))
        coord-pairs (fn [coordsx coordsy]
                      (for [row coordsx
                            col coordsy]
                        [row col]))]
    (set (map (fn [x] (value-at board x)) (coord-pairs rangex rangey)))))

(defn valid-values-for [board coords]
  (if (has-value? board coords)
    #{}
    (set/difference
     #{1 2 3 4 5 6 7 8 9}
     (reduce set/union #{} [(block-values board coords)
                            (row-values board coords)
                            (col-values board coords)]))))

(defn filled? [board]
  (some #(contains? % 0) row-values))

(defn rows [board]
  nil)

(defn valid-rows? [board]
  nil)

(defn cols [board]
  nil)

(defn valid-cols? [board]
  nil)

(defn blocks [board]
  nil)

(defn valid-blocks? [board]
  nil)

(defn valid-solution? [board]
  nil)

(defn set-value-at [board coord new-value]
  nil)

(defn find-empty-point [board]
  nil)

(defn solve [board]
  nil)
