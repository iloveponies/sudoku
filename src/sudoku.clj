(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= (value-at board coord) 0)))

(defn row-values [board coord]
  (let [[row _] coord]
    (reduce (fn [values col] (conj values (value-at board [row col]))) #{} (range 9))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (reduce (fn [values row] (conj values (value-at board [row col]))) #{} (range 9))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board coord]
  (let [[row col] coord
        tl-row (- row (mod row 3))
        tl-col (- col (mod col 3))]
    (reduce (fn [values [r c]] (conj values (value-at board [(+ tl-row r) (+ tl-col c)]))) #{} (coord-pairs [0 1 2]))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
  (set/difference
   all-values
   (set/union
    (row-values board coord)
    (col-values board coord)
    (block-values board coord)))))

(defn filled? [board]
  (loop [rows board]
    (cond
     (empty? rows) true
     (contains? (set (first rows)) 0) false
     :else (recur (rest rows)))))

(defn valid-sets? [sets]
  (loop [unread sets]
    (cond
     (empty? unread) true
     (contains? (first unread) 0) false
     :else (recur (rest unread)))))

(defn rows [board]
  (reduce (fn [rows row] (conj rows (row-values board [row 0]))) [] (range 9)))

(defn valid-rows? [board]
  (valid-sets? (rows board)))

(defn cols [board]
  (reduce (fn [cols col] (conj cols (col-values board [0 col]))) [] (range 9)))

(defn valid-cols? [board]
  (valid-sets? (cols board)))

(defn blocks [board]
  (reduce (fn [blocks [row col]] (conj blocks (block-values board [(* row 3) (* col 3)]))) [] (coord-pairs [0 1 2])))

(defn valid-blocks? [board]
  (valid-sets? (blocks board)))

(defn valid-solution? [board]
  nil)

(defn set-value-at [board coord new-value]
  nil)

(defn find-empty-point [board]
  nil)

(defn solve [board]
  nil)
