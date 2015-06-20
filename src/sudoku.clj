(ns sudoku
  (:require [clojure.set :as set]))

(def all-values #{1 2 3 4 5 6 7 8 9})
(def board identity)
(comment def solved-board (board [[1 2 3 4 5 6 7 8 9]
[1 2 3 4 5 6 7 8 9]
[1 3 2 4 5 6 7 8 9]
[1 2 3 4 5 6 7 8 9]
[1 2 9 4 5 6 7 8 3]
[1 2 3 4 0 6 7 8 9]
[1 2 3 4 5 6 7 8 9]
[1 2 3 4 5 6 7 8 9]
[1 2 3 4 5 6 7 8 0]]))
(comment def sudoku-board
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

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

(defn coord-pairs 
  ([coords] (for [p coords
        q coords]
    [p q]))
  ([coord1 coord2] (for [p coord1
                        q coord2]
                    [p q])))


(defn block-values [board coord]
  (let [bordcoord (fn[section] (range (* section 3) (* (inc section) 3)))
        sect (fn[coord] (int (/ coord 3)))
        [row col] coord
        rowcords (bordcoord (sect row))
        colcords (bordcoord (sect col))]
    (reduce #(conj %1 (value-at board %2)) #{} (coord-pairs rowcords colcords))
    ))


(defn valid-values-for [board coord]
  (if (= 0 (value-at board coord))
    (set/difference all-values (set/union (row-values board coord) (col-values board coord) (block-values board coord)))
    #{}))


(defn rows [board]
  (for [r (range 0 9)]
    (row-values board [r 0])))

(defn blocks [board]
  (reduce #(conj %1 (block-values board %2)) [] (coord-pairs [0 3 6])))

(defn cols [board]
  (rows (apply mapv vector board)))

(defn filled? [board]
  (reduce #(and (not (contains? %2 0)) %1) true (rows board)))

(defn valid? [func]
  (fn[board] (reduce #(and %1 (empty? (set/difference all-values %2))) true (func board))))

(defn valid-rows?[board] ((valid? rows) board))

(defn valid-cols? [board]
  ((valid? cols) board))

(defn valid-blocks? [board]
  ((valid? blocks) board))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [allcoords (coord-pairs (range 0 9))]
    (let [coord (first allcoords)]
      (if (empty? allcoords) nil
        (if (has-value? board coord)
          (recur (rest allcoords))
          coord))))) 

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board) [board]
    [board])
    (let [coord (find-empty-point board)]
     (for [poval (valid-values-for board coord)]
      (solve-helper (set-value-at board coord poval)))))) 
(defn solve[board]
  (first (solve-helper board)))
(solve-helper sudoku-board)
