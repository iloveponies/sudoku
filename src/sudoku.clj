(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (= (value-at board coord) 0) false true))

(defn row-values [board coord]
  (let [[x y] coord] (set (get board x))))

(defn col-values [board coord]
  (let [[x y] coord] (set (map (fn [z] (get z y)) board))))

(defn coord-pairs [coords]
  (for [x coords y coords] (vec [x y])))

(defn block-values [board coord]
  (let [[x y] coord
        xx (* 3 (int (/ x 3)))
        yy (* 3 (int (/ y 3)))
        ]
    (set (for [x2 (range xx (+ 3 xx)) y2 (range yy (+ 3 yy))]
           (value-at board [x2 y2])))
    ))

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{} (clojure.set/difference all-values
                                                 (row-values board coord)
                                                 (col-values board coord)
                                                 (block-values board coord))))

(defn filled? [board]
  (not (some zero? (for [x (range 0 9) y (range 0 9)] (value-at board [x y])))))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? (fn [x] (= all-values x)) (rows board)))

(defn cols [board]
  (map (fn [x] (col-values board [0 x])) (range 9)))

(defn valid-cols? [board]
  (every? (fn [x] (= all-values x)) (cols board)))

(defn blocks [board]
  (for [x [0 3 6] y [0 3 6]] (block-values board [x y])))

(defn valid-blocks? [board]
  (every? (fn [x] (= all-values x)) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  nil)

(defn solve [board]
  nil)
