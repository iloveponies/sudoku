(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[row col] coord
        num-cols  (count (get board 0))
        rows      (repeat num-cols row)
        cols      (range num-cols)
        coords    (map vector rows cols)
        get-coord (fn [x] (value-at board x))]
    (set (map get-coord coords))))

(defn col-values [board coord]
  (let [[row col] coord
        num-rows  (count board)
        cols      (repeat num-rows col)
        rows      (range num-rows)
        coords    (map vector rows cols)
        get-coord (fn [x] (value-at board x))]
    (set (map get-coord coords))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
       [row col]))

(defn block-values [board coord]
  (let [top-left-corner (fn [[row col]]
                            (let [block-row (quot row 3)
                                  block-col (quot col 3)]
                              [(* block-row 3)
                               (* block-col 3)]))
        [top-left-row
         top-left-col]  (top-left-corner coord)
        bot-right-row   (+ 3 top-left-row)
        bot-right-col   (+ 3 top-left-col)]
    (set (for [row (range top-left-row
                          bot-right-row)
               col (range top-left-col
                          bot-right-col)]
              (value-at board [row col])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [valid-row-values (row-values board coord)
          valid-col-values (col-values board coord)
          valid-box-values (block-values board coord)]
      (set/difference all-values
        valid-row-values
        valid-col-values
        valid-box-values))))

(defn filled? [board]
  (let [all-numbers-set (set (apply concat board))]
    (not (contains? all-numbers-set 0))))

(defn rows [board]
  (let [num-rows (count board)]
    (for [r (range num-rows)]
         (row-values board [r 0]))))

(defn valid-rows? [board]
  (let [row-sets (rows board)
        num-rows (count row-sets)]
    (= row-sets
       (repeat num-rows all-values))))

(defn cols [board]
  (let [num-cols (count (get board 0))]
    (for [c (range num-cols)]
         (col-values board [0 c]))))

(defn valid-cols? [board]
  (let [col-sets (cols board)
        num-cols (count col-sets)]
    (= col-sets
       (repeat num-cols all-values))))

(defn blocks [board]
  (for [block-row (range 3)
        block-col (range 3)]
       (let [row (* block-row 3)
             col (* block-col 3)]
         (block-values board [row col]))))

(defn valid-blocks? [board]
  (let [box-sets  (blocks board)
        num-boxes (count box-sets)]
    (= box-sets
       (repeat num-boxes all-values))))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [num-rows (count board)
        rows     (range num-rows)
        coords   (coord-pairs rows)]
    (first (filter (fn [coord] (not (has-value? board coord)))
                   coords))))

(defn solve-helper [board]
  (if (valid-solution? board)
    [board]
    (let [empty-coord (find-empty-point board)]
      (for [value        (valid-values-for board empty-coord)
            solved-board (solve-helper
                           (set-value-at board empty-coord value))]
           solved-board))))

(defn solve [board]
  (first (solve-helper board)))
