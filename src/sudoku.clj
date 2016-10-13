(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  ((complement zero?) (value-at board coord)))

(defn row-values [board [row _]]
  (set (get-in board [row])))

(defn col-values [board [_ col]]
  (set (map #(get % col) board)))

(defn coord-pairs [coords]
  (vec
    (for [row coords
          col coords]
      (vector row col))))

(defn top-left [[row col]]
  [(- row (mod row 3)) (- col (mod col 3))])

(defn block-values [board coord]
  (set (map #(value-at board
                       [(+ (first (top-left coord)) (first %))
                        (+ (second (top-left coord)) (second %))]) (coord-pairs [0 1 2]))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (clojure.set/difference
      all-values
      (clojure.set/union
        (row-values board coord)
        (col-values board coord)
        (block-values board coord)))))

(defn filled? [board]
  ((complement contains?) (apply clojure.set/union (map set board)) 0))

(defn rows [board]
  (vec (clojure.set/union (map set board))))

(defn cols [board]
  (vec (map #(col-values board [0 %]) [0 1 2 3 4 5 6 7 8])))

(defn blocks [board]
  (vec (map #(block-values board %) [[0 0] [0 3] [0 6]
                                     [3 0] [3 3] [3 6]
                                     [6 0] [6 3] [6 6]])))

(defn valid-rows? [board]
  (zero? (count (filter #(< (count %) 9) (rows board)))))

(defn valid-cols? [board]
  (zero? (count (filter #(< (count %) 9) (cols board)))))

(defn valid-blocks? [board]
  (zero? (count (filter #(< (count %) 9) (blocks board)))))

(defn valid-solution? [board]
  (and
    (valid-rows? board)
    (valid-cols? board)
    (valid-blocks? board)))


(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter #((complement has-value?) board %) (coord-pairs [0 1 2 3 4 5 6 7 8]))))




(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      [])
    (for [n (valid-values-for board (find-empty-point board))
          solution (solve-helper (set-value-at board (find-empty-point board) n))]
      solution)))


(defn solve [board]
  (first (solve-helper board)))


