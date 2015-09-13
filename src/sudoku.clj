(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (let [val (value-at board coord)]
    (pos? val)))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [rows (range 9)
        [_ col] coord]
    (set (map #(value-at board [% col]) rows))))

(defn- top-left [coord]
  (let [[row col] coord
        map-to-corner (fn [x]
                        (cond
                          (< x 3) 0
                          (< x 6) 3
                          :else 6))]
    [(map-to-corner row) (map-to-corner col)]))

(defn coord-pairs [coords]
  (for [a coords
        b coords]
    [a b]))

(defn block-values [board coord]
  (let [[top left] (top-left coord)]
    (set (for [row (range top (+ top 3))
               col (range left (+ left 3))]
           (value-at board [row col])))))

(defn valid-values-for [board coord]
  (let [r-vals (row-values board coord)
        c-vals (col-values board coord)
        b-vals (block-values board coord)
        values (clojure.set/union r-vals c-vals b-vals)]
    (if (has-value? board coord)
      #{}
      (clojure.set/difference all-values values))))

(defn- board-values-as-seq [board] (apply concat board))

(defn filled? [board]
  (not (contains? (set (board-values-as-seq board)) 0)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? #(= % all-values) (rows board)))

(defn zip [& colls] (partition (count colls) (apply interleave colls)))

(defn cols [board]
  (map set (apply zip board)))

(defn valid-cols? [board]
  (every? #(= % all-values) (cols board)))

(defn blocks [board]
  (reduce #(conj %1 (block-values board %2)) [] (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (every? #(= % all-values) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-blocks? board) (valid-cols? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [coords (coord-pairs (range 9))]
    (if
      (not (has-value? board (first coords))) (first coords)
                                              (recur (rest coords)))))

(defn solve [board]
  nil)
