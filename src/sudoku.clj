(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [row col]]
  (set (board row)))

(defn col-values [board [row col]]
  (set (map (fn [row] (row col)) board)))

(defn coord-pairs [coords]
  (for [cod1 coords
        cod2 coords]
    [cod1 cod2]))

(def top-left-coords (coord-pairs [0 3 6]))

(defn top-left [[row col]]
  (some (fn [[rrow ccol]]
          (if (and (<= 0 (- row rrow) 2)
                   (<= 0 (- col ccol) 2))
            [rrow ccol]
            false))
        top-left-coords))

(defn block-values [board coord]
  (let [[row col] (top-left coord)
        row-seq (range row (+ 3 row))
        col-seq (range col (+ 3 col))]
    (->> [rrow ccol]
         (for [rrow row-seq
               ccol col-seq])
         (map (fn [cod] (value-at board cod)))
         set)))

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
      (let [row-set (row-values board coord)
            col-set (col-values board coord)
            blk-set (block-values board coord)]
        (set/difference all-values
                        (set/union row-set col-set blk-set)))))

(defn filled? [board]
  (let [board-set (set (flatten board))]
    (not (contains? board-set 0))))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? (fn [ss] (empty? (set/difference all-values ss)))
          (rows board)))

(defn cols [board]
  (map (fn [col]
         (->> board
              (map (fn [row] (row col)))
              set))
       (range 0 9)))

(defn valid-cols? [board]
  (every? (fn [ss] (empty? (set/difference all-values ss)))
          (cols board)))

(defn blocks [board]
  (map (fn [coord] (set (block-values board coord)))
       top-left-coords))

(defn valid-blocks? [board]
  (every? (fn [ss] (empty? (set/difference all-values ss)))
          (blocks board)))

(defn valid-solution? [board]
  (and (valid-cols? board) (valid-rows? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (some (fn [coord] (if (has-value? board coord)
                      false
                      coord))
        (coord-pairs (range 0 9))))

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      [])
    (let [location (find-empty-point board)]
      (for [value (valid-values-for board location)
            sol (solve-helper (set-value-at board location value))]
        sol))))

(defn solve [board]
  (first (solve-helper board)))
