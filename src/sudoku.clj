(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (== (value-at board coord) 0)))

(defn row-values [board [row _]]
  (set (nth board row)))

(defn col-values [board [_ col]]
  (let [f (fn [x] (nth x col))]
    (set (map f board))))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn top-left [coord]
  (map (fn [x] (* (int (/ x 3)) 3)) coord))

(defn block-values [board coord]
  (let [f (fn [x] (value-at board (map + (top-left coord) x)))]
    (set (map f (coord-pairs [0 1 2])))))

(defn valid-values-for [board coord]
  (let [row-values (row-values board coord)
        col-values (col-values board coord)
        blk-values (block-values board coord)]
    (if (has-value? board coord)
      #{}
      (set/difference all-values row-values col-values blk-values))))

(defn filled? [board]
  (let [all-coords      (coord-pairs (range 0 9))
        board-vals      (map (fn[x] (value-at board x)) all-coords)
        uniq-board-vals (set board-vals)]
    (not (contains? uniq-board-vals 0))))

(defn rows [board]
  (for [x (range 0 9)]
    (row-values board [x 0])))

(defn cols [board]
  (for [y (range 0 9)]
    (col-values board [0 y])))

(defn blocks [board]
  (let [pred?  (fn [x] (= (top-left x) x))
        blocks (filter pred? (coord-pairs (range 0 9)))]
    (map (fn [block] (block-values board block)) blocks)))

(defn valid-rows? [board]
  (let [rows   (rows board)
        valid? (fn [x] (= (seq all-values) (seq x)))]
    (reduce (fn [x y] (and x y)) (map valid? rows))))

(defn valid-cols? [board]
  (let [cols (cols board)
        valid? (fn [x] (= (seq all-values) (seq x)))]
    (reduce (fn [x y] (and x y)) (map valid? cols))))

(defn valid-blocks? [board]
  (let [blocks (blocks board)
        valid? (fn [x] (= (seq all-values) (seq x)))]
    (reduce (fn [x y] (and x y)) (map valid? blocks))))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [pred? (fn [coord] (== 0 (value-at board coord)))]
    (first (filter pred? (coord-pairs (range 0 9))))))

(defn solve [board]
  (if (valid-solution? board)
    board
    (let [empty-point     (find-empty-point board)
          possible-values (valid-values-for board empty-point)]
      (for [possible-value possible-values
            solution       (solve (set-value-at board empty-point possible-value))]
        solution))))
