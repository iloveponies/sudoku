(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (> (value-at board coord) 0))

(defn row-values [board coord]
  (into #{} (get board (first coord))))

(defn col-values [board coord]
  (into #{} (map #(get %1 (second coord)) board )))

(defn coord-pairs [coords]
  (for [row coords
        col coords ]
    [row col]))

(defn all-coords [coord]
  (let [[top left] (map #(- %1 (rem %1 3))  coord)]
    (for [row (range top (+ top 3))
          col (range left (+ left 3))]
      [row col])))

(defn block-values [board coord]
  (set (map #(value-at board %1) (all-coords coord))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                    (row-values board coord)
                    (col-values board coord)
                    (block-values board coord))))

(defn filled? [board]
  (not (contains? (set (apply concat board)) 0)))

(defn get-not-valid [seq]
  (filter #(not (empty? (set/difference all-values %1))) seq))

(defn rows [board]
  (map #(set %1) board))

(defn valid-rows? [board]
  (empty? (get-not-valid (rows board))))

(defn cols [board]
 (map #(col-values board [0 %1]) (range 9)))


(defn valid-cols? [board]
  (empty? (get-not-valid (cols board))))

(defn blocks [board]
   (map #(block-values board %1) (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (empty? (get-not-valid (blocks board))))

(defn valid-solution? [board]
  (and
    (valid-blocks? board)
    (valid-cols? board)
    (valid-rows? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter #(zero? (value-at board %1)) (coord-pairs (range 9)))))


(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      [])
    (let [coord (find-empty-point board)]
      (for [value (valid-values-for board coord)
            solution (solve (set-value-at board coord value))]
        solution))))
