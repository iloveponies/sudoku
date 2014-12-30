(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [row _]]
  (set (get board row)))

(defn col-values [board [_ col]]
  (set (map (fn [row] (get row col)) board)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    (vector row col)))

(defn coord-pairs-from [[row col]]
  (let [rows (range row (+ 3 row))
        cols (range col (+ 3 col))]
    (for [row rows
          col cols]
      (vector row col))))

(defn upper-corner [[row col]]
  (let [shift (fn [x] (cond
                        (< x 3) 0
                        (< x 6) 3
                        :else 6))]
    (vector (shift row) (shift col))))

(defn block-values [board coord]
  (set (map (fn [x] (get-in board x)) (coord-pairs-from (upper-corner coord)))))

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
    (let [v-block (block-values board coord)
          v-col (col-values board coord)
          v-row (row-values board coord)]
      (set/difference all-values (set/union v-block v-col v-row)))))

(defn filled? [board]
  (not (contains? (set (apply concat board)) 0)))

(defn rows [board]
  (map set board))

(defn valid? [pred]
  (reduce (fn [acc n] (and acc (= n all-values))) true pred))

(defn valid-rows? [board]
  (valid? (rows board)))

(defn cols [board]
  (map (fn [x] (col-values board [0 (dec x)])) all-values))

(defn valid-cols? [board]
  (valid? (cols board)))

(defn blocks [board]
  (map (fn [x] (block-values board x)) (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (valid? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [row 0
         col 0]
    (if (== 0 (value-at board [row col]))
      [row col]
      (recur (if (== col 8) (inc row) row)
             (if (== col 8) 0 (inc col))))))

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      [])
    (let [point (find-empty-point board)
          values (valid-values-for board point)]
          (for [value values
                solution (solve-helper (set-value-at board point value))]
            solution))))

(defn solve [board]
  (first (solve-helper board)))
