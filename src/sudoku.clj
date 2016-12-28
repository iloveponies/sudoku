(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (zero? (value-at board coord))
    false
    true))

(defn row-values [board coord]
  (let [[row col] coord]
    (into #{} (get board row))))

(defn board-size [board]
  (count board))

(defn col-values [board coord]
  (let [[row col] coord]
    (reduce #(conj %1 (get %2 col)) #{} board)))

(defn coord-pairs
  ([coords]
   (for [row coords
         col coords]
     [row col]))
  ([coord1 coord2]
   (for [row coord1
         col coord2]
     [row col])))

(defn top-left-coords [coord]
  (let [[row col] coord
        x (* 3 (int (/ row 3)))
        y (* 3 (int (/ col 3)))]
    [x y]))

(defn block-values [board coord]
  (let [[x y] (top-left-coords coord)
        block (coord-pairs (range x (+ 3 x)) (range y (+ 3 y)))]
    (reduce #(conj %1 (value-at board %2)) #{} block)))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [all-values (into #{} (range 1 10))
          block (block-values board coord)
          row (row-values board coord)
          col (col-values board coord)
          available (set/difference all-values block row col)]
      available)))

(defn filled? [board]
  (let [present-values (apply set/union (map #(into #{} %) board))]
    (not (contains? present-values 0))))

(defn rows [board]
  (map #(into #{} %) board))

(def valid-set (into #{} (range 1 10)))

(defn valid-rows? [board]
  (every? #(= valid-set %) (rows board)))

(defn transpose [board]
  (let [nth-col (fn [n] (map #(nth % n) board))]
    (reduce #(conj %1 (nth-col %2)) [] (range 9))))

(defn cols [board]
  (let [board' (transpose board)]
    (map #(into #{} %) board')))

(defn valid-cols? [board]
  (every? #(= valid-set %) (cols board)))

(defn blocks [board]
  (for [x (range 0 9 3)
        y (range 0 9 3)]
    (block-values board [x y])))

(defn valid-blocks? [board]
  (every? #(= valid-set %) (blocks board)))

(defn valid-solution? [board]
  (reduce #(and %1 (%2 board)) true [valid-rows? valid-cols? valid-blocks?]))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [all-indices (coord-pairs (range 9))
        iboard (map vector all-indices (apply concat board))]
    (first (first (filter #(zero? (second %)) iboard)))))

(defn solve* [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      [])
    (let [empty-point (find-empty-point board)]
        (for [possibility (valid-values-for board empty-point)
              solution (solve* (set-value-at board empty-point possibility))]
          solution))))

(defn solve [board]
  (first (solve* board)))
