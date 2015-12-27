(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= (value-at board coord) 0)))

(defn row-values [board [row col]]
  (set (get board row)))

(defn col-values [board [row col]]
  (set (map (fn [r] (get r col)) board)))

(defn coord-pairs [coords]
  (for [row coords, col coords] [row, col]))

(defn block-corner [coords]
  (map (fn [x] (* 3 (int (/ x 3)))) coords))

(defn block-values [board coord]
   (let [[r0 c0] (block-corner coord)
         rel-coords (coord-pairs [0 1 2])
         block-pairs (map (fn [[r c]] [(+ r r0) (+ c c0)]) rel-coords)]
     (set (for [rc block-pairs] (value-at board rc)))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [row-vals (row-values board coord)
               col-vals (col-values board coord)
               blk-vals (block-values board coord)
               usd-vals (set/union row-vals col-vals blk-vals)
               all-vals (set (range 1 10))]
           (set/difference all-vals usd-vals))))

(defn filled? [board]
  (let [vals (set (apply concat board))]
    (not (contains? vals 0))))

(defn rows [board]
(for [r (range 0 9)] (row-values board [r 0])))

(defn valid-rows? [board]
  (let [all-vals (set (range 1 10))]
    (every? (fn [s] (= s all-vals)) (rows board))))

(defn cols [board]
 (for [c (range 0 9)] (col-values board [0 c])))

(defn valid-cols? [board]
  (let [all-vals (set (range 1 10))]
    (every? (fn [s] (= s all-vals)) (cols board))))

(defn blocks [board]
  (for [r [0 3 6] c [0 3 6]] (block-values board [r c])))

(defn valid-blocks? [board]
  (let [all-vals (set (range 1 10))]
    (every? (fn [s] (= s all-vals)) (blocks board))))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (fn [rc] (= 0 (value-at board rc))) (coord-pairs (range 0 9)))))

(defn solve-helper [board]
 (if (filled? board)
   (if (valid-solution? board) [board] ())
   (let [coord (find-empty-point board)
         vals  (valid-values-for board coord)]
     (for [val      vals
           solution (solve-helper (set-value-at board coord val))]
       solution))))

(defn solve [board]
  (first (solve-helper board)))
