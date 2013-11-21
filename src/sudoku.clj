(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[row col] coord] (set (get board row))))

(defn col-values [board coord]
  (let [[row col] coord] (set (for [row (range 0 9)] (value-at board [row col])))))

(defn coord-pairs [coords]
  (vec (for [row coords col coords] [row col])))

(defn block-values [board coord]
  (let [[r c] coord [cr cc] [(* (int (/ r 3)) 3) (* (int (/ c 3)) 3)]]
    (set (for [row (range cr (+ cr 3)) col (range cc (+ cc 3))] (value-at board [row col])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
    (set/difference all-values (set/union (row-values board coord) (col-values board coord) (block-values board coord)))))

(defn filled? [board]
  (not (contains? (set (apply concat board)) 0)))

(defn rows [board]
  (for [row (range 0 9)] (row-values board [row 0])))

(defn valid-rows? [board]
  (every? (fn [x] (= x all-values)) (rows board)))

(defn cols [board]
  (for [col (range 0 9)] (col-values board [0 col])))

(defn valid-cols? [board]
  (every? (fn [x] (= x all-values)) (cols board)))

(defn blocks [board]
  (for [row (range 0 3) col (range 0 3)] (block-values board [(* row 3) (* col 3)])))

(defn valid-blocks? [board]
  (every? (fn [x] (= x all-values)) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (fn [x] (zero? (value-at board x))) (coord-pairs (range 0 9)))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board) board)
    (let [point (find-empty-point board)]
      (for [value (valid-values-for board point)
            solve2 (solve (set-value-at board point value))]
        solve2))))
