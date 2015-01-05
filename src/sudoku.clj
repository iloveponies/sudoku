(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (get-in board coord))))

(defn row-values [board [r _]]
  (set (get board r)))

(defn col-values [board [_ c]]
  (let [g (fn [row] (get row c))]
    (set (map g board))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn coord-block [[x y]]
  [ (- x (mod x 3)) (- y (mod y 3)) ])

(defn block-values [board coord]
  (let [[x y] (coord-block coord)
        coords (for [r (range x (+ x 3))
                     c (range y (+ y 3))]
                 [r c])]
  (set (map (fn [co] (value-at board co)) coords))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (reduce set/difference (set (range 1 10))
            [(block-values board coord)
             (col-values board coord)
             (row-values board coord)])))

(defn filled? [board]
  (let [notempty (fn [x] (not (= 0 x)))
        f (fn [row] (every? notempty row))]
    (every? f board)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? (fn [x] (= 9 (count x))) (rows board)))

(defn cols [board]
  (let [go (fn [i] (col-values board [nil i]))]
    (map go (range 0 9))))

(defn valid-cols? [board]
  (every? (fn [x] (= 9 (count x))) (cols board)))

(defn blocks [board]
  (map (fn [x] (block-values board x))
       (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (every? (fn [x] (= 9 (count x))) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [f (fn [x] (not (has-value? board x)))]
    (first (filter f (for [x (range 0 9) y (range 0 9)] [x y])))))

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board) [board] [])
    (let [free (find-empty-point board)]
      (for [value (valid-values-for board free)
            altered (solve-helper (set-value-at board free value))]
        altered))))

(defn solve [board]
  (first (solve-helper board)))
