(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (= 0 (value-at board coord))
    false
    true))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (set (map (fn [row] (get row (second coord))) board)))

(defn coord-pairs [coords]
  (for [a coords
        b coords]
    [a b]))

(defn block-values [board [y x]]
 (let [row-begin (- y (mod y 3))
       col-begin (- x (mod x 3))]
   (set (map
         (fn [[row col]] (let [coords [(+ row-begin row) (+ col-begin col)]]
                           (value-at board coords)))
         (coord-pairs [0 1 2])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [not-possible (set/union (row-values board coord) (col-values board coord) (block-values board coord))]
      (set/difference all-values not-possible))))

(defn filled? [board]
  (every? #(has-value? board %) (coord-pairs (range 9))))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? true?
   (for [a (range 0 9)]
     (= (set (row-values board [a 0])) all-values))))

(defn cols [board]
  (map set (for [col (range 9)]
             (col-values board [0 col]))))

(defn valid-cols? [board]
  (every? true?
   (for [a (range 0 9)]
     (= (set (col-values board [0 a])) all-values))))

(defn blocks [board]
  (for [a [0 3 6]
        b [0 3 6]]
    (block-values board [a b])))

(defn valid-blocks? [board]
  (every? true?
   (for [a [0 3 6]
         b [0 3 6]]
     (= (set (block-values board [a b])) all-values))))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (fn [coord] (not (has-value? board coord))) (coord-pairs (range 9)))))

(defn solve-helper [board]
  (let [e (find-empty-point board)]
    (if (nil? e)
      (if (valid-solution? board)
        [board]
        [])
      (for [v (valid-values-for board e)
            s (solve-helper (set-value-at board e v))]
        s))))

(defn solve [board]
  (first (solve-helper board)))

