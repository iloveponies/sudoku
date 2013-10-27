(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= (value-at board coord) 0))

(defn row-values [board coord]
  (let [[row col] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[row col] coord]
    (set (map (fn [r] (r col)) board))))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-coords [coords]
  (let [[x y] coords
        a     (* 3 (int (/ x 3)))
        b     (* 3 (int (/ y 3)))]
    (for [i (range a (+ 3 a))
          j (range b (+ 3 b))]
      [i j])))

(defn block-values [board coord]
  (set (map (fn [c] (value-at board c)) (block-coords coord))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference
      all-values
      (block-values board coord)
      (row-values board coord)
      (col-values board coord))))

(defn filled? [board]
  (not (contains? (set (apply concat board)) 0)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? (fn [x] (= x all-values)) (rows board)))

(defn cols [board]
  (for [i (range 0 9)]
    (col-values board [0 i])))

(defn valid-cols? [board]
 (every? (fn [x] (= x all-values)) (cols board)))

(defn blocks [board]
  (map (fn [x] (block-values board x)) (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (every? (fn [x] (= x all-values)) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter
           (fn [x]
             (if (= 0 (value-at board x))
               x
               nil))
           (coord-pairs (range 9)))))

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      [])
    (let [pt (find-empty-point board)]
      (for [v   (valid-values-for board pt)
            sol (solve-helper (set-value-at board pt v))]
        sol))))

(defn solve [board]
  (first (solve-helper board)))
