(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (== 0 (value-at board coord))))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (set (map (fn [b] (get b col)) board))))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-values [board coord]
  (let [block-values-helper (fn [[x y]]
                              (vector (* (quot x 3) 3)
                                       (* (quot y 3) 3)))
        [start-x start-y]   (block-values-helper coord)]
    (set (for [x (range start-x (+ start-x 3))
          y (range start-y (+ start-y 3))]
      (value-at board [x y])))))

(defn valid-values-for [board coord]
    (if (== (value-at board coord) 0)
      (set/difference
        all-values
        (set/union (row-values board coord)
                   (col-values board coord)
                   (block-values board coord)))
      #{}))

(defn filled? [board]
  (let [helper (fn [b]
                 (apply concat board))]
    (not (contains? (set (helper board)) 0))))

(defn rows [board]
  (for [row (range 9)]
    (set (row-values board [row 0]))))

(defn valid-rows? [board]
  (every? (fn [x] (= x all-values)) (rows board)))

(defn cols [board]
  (for [col (range 9)]
    (set (col-values board [0 col]))))

(defn valid-cols? [board]
  (every? (fn [x] (= x all-values)) (cols board)))

(defn blocks [board]
  (for [row [0 3 6]
        col [0 3 6]]
    (set (block-values board [row col]))))

(defn valid-blocks? [board]
  (every? (fn [x] (= x all-values)) (blocks board)))

(defn valid-solution? [board]
  (and
    (valid-rows? board)
    (valid-cols? board)
    (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [row 0
         col 0]
    (cond
      (== (value-at board [row col]) 0)
        [row col]
      (and (== row 8) (== col 8))
        nil
      (== col 8)
        (recur (inc row) 0)
      :else
        (recur row (inc col)))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      [])
  (let [empty-square (find-empty-point board)]
    (for [next-value (valid-values-for board empty-square)
          solution (solve (set-value-at board empty-square next-value))]
      solution))))
