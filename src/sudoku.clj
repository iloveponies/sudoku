(ns sudoku
  (:require [clojure.set :as set]))

; Definitions of the data structures and helper functions
(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

; Functions to implement that make tests pass:
(defn value-at [board coord]
 (get-in board coord))

(defn has-value? [board coord]
 (not (= 0 (value-at board coord))))

(defn row-values [board coord]
 (let [[row-number _] coord
       row (get board row-number)]
  (set row)))

(defn col-values [board coord]
 (let [[_ col-number] coord
       column (map #(get % col-number) board)]
  (set column)))

(defn coord-pairs [coords]
 (for [row coords
       col coords]
  [row col]))

(defn corner-number [n]
  (cond
   (<= 0 n 2) 0
   (<= 3 n 5) 3
   (<= 6 n 8) 6
   :else false))

(defn block-corner [coord]
 (map corner-number coord))

(defn block-values [board coord]
 (let [[coord-corner-x coord-corner-y] (block-corner coord)
       block-values-list (for [x (range coord-corner-x (+ coord-corner-x 3))
                               y (range coord-corner-y (+ coord-corner-y 3))]
                          (value-at board [x y]))]
  (set block-values-list)))

(defn valid-values-for [board coord]
 (if (has-value? board coord)
  #{}
  (set/difference all-values
                  (row-values board coord)
                  (col-values board coord)
                  (block-values board coord))))

(defn filled? [board]
 (let [all-values-concat (reduce concat [] board)
       all-values-concat-set (set all-values-concat)
       board-has-empty-value (contains? all-values-concat-set 0)]
  (not board-has-empty-value)))

(defn rows [board]
 (mapv set board))

(defn cols [board]
 (rows (vec (apply map vector board))))

(defn blocks [board]
 (let [block-coords (coord-pairs [0 3 6])]
  (mapv #(block-values board %) block-coords)))

(defn valid-cut [board cut-fn]
 (let [group (cut-fn board)]
  (reduce #(and %1 (= %2 all-values))
          true
          group)))

(defn valid-rows? [board]
 (valid-cut board rows))

(defn valid-cols? [board]
 (valid-cut board cols))

(defn valid-blocks? [board]
 (valid-cut board blocks))

(defn valid-solution? [board]
 (and (valid-rows? board)
      (valid-cols? board)
      (valid-blocks? board)))

(defn set-value-at [board coord new-value]
 (assoc-in board coord new-value))

(defn empty-point-finder [board coords]
 (let [first-coord (first coords)
       pending-coords (rest coords)]
  (cond
   (empty? coords) nil
   (has-value? board first-coord) (empty-point-finder board pending-coords)
   (not (has-value? board first-coord)) first-coord)))

(defn find-empty-point [board]
 (let [all-coords (coord-pairs [0 1 2 3 4 5 6 7 8])]
  (empty-point-finder board all-coords)))

(defn solve-helper [solution]
 (if (filled? solution)
  (if (valid-solution? solution)
   [solution]
   [])
  (let [empty-point (find-empty-point solution)]
   (for [valid-value (valid-values-for solution empty-point)
         patched-solution (solve-helper (set-value-at solution empty-point valid-value))]
    patched-solution))))

(defn solve [board]
 (first (solve-helper board)))
