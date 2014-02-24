(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (pos? (value-at board coord)))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (set (map #(get % (second coord)) board)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-top-left [coord]
  (mapv #(- % (mod % 3)) coord))

(defn block-coords [coord]
  (let [top-left (block-top-left coord)
        square-coords (coord-pairs [0 1 2])]
    (map #(mapv + top-left %) square-coords)))

(defn block-values [board coord]
  (set (map #(value-at board %) (block-coords coord))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                    (row-values board coord)
                    (col-values board coord)
                    (block-values board coord))))

(defn all-board-values [board]
  (apply concat board))

(defn filled? [board]
  (not (contains? (set (all-board-values board)) 0)))

(defn rows [board]
  (for [row (range 9)]
    (row-values board [row 0])))

(defn valid-rows? [board]
  (= (set (rows board)) #{all-values}))

(defn cols [board]
  (for [cols (range 9)]
    (col-values board [0 cols])))

(defn valid-cols? [board]
  (= (set (cols board)) #{all-values}))

(defn blocks [board]
  (for [coord (coord-pairs [0 3 6])]
    (block-values board coord)))

(defn valid-blocks? [board]
  (= (set (blocks board)) #{all-values}))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [all-points (coord-pairs (range 9))]
    (first (remove #(has-value? board %) all-points))))

(defn solve-helper [board]
  (cond (valid-solution? board) [board]
        (filled? board) nil
        :else
        (let [coord (find-empty-point board)]
          (for [value (valid-values-for board coord)
                solution (solve-helper (set-value-at board coord value))]
            solution))))

(defn solve [board]
  (first (solve-helper board)))
