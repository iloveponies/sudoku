(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(def block-coord-list
  [[0 0] [0 3] [0 6]
   [3 0] [3 3] [3 6]
   [6 0] [6 3] [6 6]])

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= (value-at board coord) 0))

(defn row-values [board [r _]]
  (set (get board r)))

(defn col-values [board [_ c]]
  (set (map #(value-at board [%1 c]) (range 9))))

(defn coord-pairs [coords]
  (for [c1 coords
        c2 coords]
    (vec [c1 c2])))

(defn int-div [a b]
  (int (/ a b)))

(defn block-coords [[r c]]
  (for [[a b] (coord-pairs [0, 1, 2])]
    (identity 
      [(+ a (* (int-div r 3) 3))
       (+ b (* (int-div c 3) 3))])))

(defn block-values [board coord]
  (set (map #(value-at board %1) (block-coords coord))))

(defn valid-values-for [board coord]
  (if (not= (value-at board coord) 0)
    #{}
    (set/difference all-values
      (block-values board coord)
      (row-values board coord)
      (col-values board coord))))

(defn filled? [board]
  (every? #(not= (value-at board %1) 0) (coord-pairs (range 9))))

(defn rows [board]
  (map #(row-values board [%1 0]) (range 9)))

(defn valid-rows? [board]
  (every? #(= all-values %1) (rows board)))

(defn cols [board]
  (map #(col-values board [0 %1]) (range 9)))

(defn valid-cols? [board]
  (every? #(= all-values %1) (cols board)))

(defn blocks [board]
  (map #(block-values board %1) block-coord-list))

(defn valid-blocks? [board]
  (every? #(= all-values %1) (blocks board)))

(defn valid-solution? [board]
  (and (valid-blocks? board) (valid-cols? board) (valid-rows? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter #(zero? (value-at board %1)) (coord-pairs (range 9)))))

(defn new-solve [board coord]
  (for [new-value (valid-values-for board coord)]
    (solve (set-value-at board coord new-value))))

(defn solve [board]
  (if (and (filled? board) (valid-solution? board)) ; maybe filled? makes it a bit faster
    board
    (let [coord (find-empty-point board)]
      (if (nil? coord)
        nil
        (first (filter boolean (new-solve board coord)))))))