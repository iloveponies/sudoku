(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(def suboard
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(row-values suboard [0 0])
(row-values suboard [0 2])
(row-values suboard [5 2])

(defn col-values [board coord]
  (let [[_ col] coord]
    (reduce #(conj %1 (get %2 col)) #{} board)))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block [xblock yblock]
  (for [[x y] (coord-pairs [0 1 2])]
    [(+ (* 3 xblock) x) (+ (* 3 yblock) y)]))

(defn block-at [coord]
  (let [[x y] coord]
    (block (quot x 3) (quot y 3))))

(defn block-values [board coord]
  (set (for [crd (block-at coord)]
    (value-at board crd))))

(defn valid-values-for [board coord]
  (if (not= 0 (value-at board coord))
    #{}
    (set/difference all-values
                    (set/union (row-values board coord)
                               (col-values board coord)
                               (block-values board coord)))))

(def valid-values valid-values-for)

(defn filled? [board]
  (not (contains? (set (apply concat board)) 0)))

(defn rows [board]
  (map #(row-values board [% 0]) (range 0 9)))

(defn valid? [a-set]
  (empty? (drop-while #(= all-values %) a-set)))

(defn valid-rows? [board]
  (valid? (rows board)))

(defn cols [board]
  (map #(col-values board [0 %]) (range 0 9)))

(defn valid-cols? [board]
  (valid? (cols board)))

(defn blocks [board]
  (for [x (range 0 9 3)
        y (range 0 9 3)]
    (block-values board [x y])))

(defn valid-blocks? [board]
  (valid? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [x 0
         y 0]
    (cond
     (= 0 (value-at board [x y]))
       [x y]
     (> x 8)
       (recur 0 (inc y))
     (> y 8)
       nil
     :else
       (recur (inc x) y))))

(defn solve-helper [board]
  (cond
   (valid-solution? board)
     [board]
   :else
     (for [coord [(find-empty-point board)]
           new-value (valid-values board coord)
           solution (solve-helper (set-value-at board coord new-value))]
       solution)))

(defn solve [board]
  (first (solve-helper board)))
