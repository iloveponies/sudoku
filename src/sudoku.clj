(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def vals #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (== (value-at board coord) 0)))

(defn row-values [board coord]
  (let [[row col] coord] (set (get board row))))

(defn col-values [board coord]
  (let [[row col] coord
        col-get (fn [x] (get x col))]
    (set (map col-get board))))

(defn coord-pairs [coords]
  (for [p1 coords p2 coords]
    [p1 p2]))

(defn top-left [coords]
  (let [[row col] coords]
    [(- row (mod row 3)) (- col (mod col 3))]))

(defn block-values [board coord]
  (let [[r c] (top-left coord)]
    (set (for [row (range r (+ r 3))
          col (range c (+ c 3))]
      (value-at board [row col])))))

(defn valid-values-for [board coord]
  (let [[row col] coord]
    (if
      (has-value? board coord)
      #{}
      (set/difference
       vals
       (set/union
        (row-values board coord)
        (col-values board coord)
        (block-values board coord))))))

(defn filled? [board]
  (not (contains? (set (for [row (range 0 8) col (range 0 8)]
    (value-at board [row col]))) 0)))

(defn rows [board]
  (for [row (range 0 9)] (row-values board [row 0])))

(defn valid? [blocks]
  (let
    [legit?
     (fn [x]
       (empty? (set/difference vals x)))]
    (every? legit? blocks)))

(defn valid-rows? [board]
  (valid? (rows board)))

(defn cols [board]
  (for [col (range 0 9)] (col-values board [0 col])))

(defn valid-cols? [board]
  (valid? (cols board)))

(defn blocks [board]
  (for [row (range 0 3) col (range 0 3)] (block-values board [(* 3 row) (* 3 col)])))

(defn valid-blocks? [board]
  (valid? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [row 8 col 8]
    (cond
     (< row 0) nil
     (not (has-value? board [row col])) [row col]
     (> col 0) (recur row (dec col))
     :else (recur (dec row) 8))))

(defn solver [board]
  (if
    (filled? board)
    (if (valid-solution? board) [board] '())
    (let
      [t
       (find-empty-point board)]
      (for [v (valid-values-for board t) s (solver (set-value-at board t v))] s))))

(defn solve [board]
  (first (solver board)))
