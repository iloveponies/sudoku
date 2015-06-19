(ns sudoku
  (:require [clojure.set :as set]))
(apply mapv vector sudoku-board)

(apply + [1 2 3 4])
(def sudoku-board
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))
(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board coord]
  (let [[row col] coord]
    (set (get board row))))
(defn transpose[board]
  (apply mapv vector board))
(defn col-values [board coord]
  (let [[row col] coord]
    (set (get (transpose board) col))))
(coord-pairs [0 1 2])
(defn coord-pairs 
  ([coords] (for [p coords
        q coords]
    [p q]))
  ([coord1 coord2] (for [p coord1
                        q coord2]
                    [p q])))

(block-values sudoku-board [8 8])
(defn block-values [board coord]
  (let [bordcoord (fn[section] (range (* section 3) (* (inc section) 3)))
        sect (fn[coord] (int (/ coord 3)))
        [row col] coord
        rowcords (bordcoord (sect row))
        colcords (bordcoord (sect col))]
    (reduce #(conj %1 (value-at board %2)) #{} (coord-pairs rowcords colcords))
    ))
(block-values sudoku-board [0 2])
(defn valid-values-for [board coord]
  nil)

(defn filled? [board]
  nil)

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
