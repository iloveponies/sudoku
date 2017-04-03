(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn row-coord [coord]
  (nth coord 0))

(defn col-coord [coord]
  (nth coord 1))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (set (get board (row-coord coord ))))

(defn col-values [board coord]
  (let [column (col-coord coord)
        add-row-value-to-set (fn [current-set row]
                               (conj current-set (value-at board [row column])))]
    (reduce add-row-value-to-set #{} (range 0 9))))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn top-left-block []
  (coord-pairs [0 1 2]))

(defn increase-row-coord [coord-seq value]
  (for [coord coord-seq] [(+ (nth coord 0) value) (nth coord 1)]))

(defn increase-col-coord [coord-seq value]
  (for [coord coord-seq] [(nth coord 0) (+ (nth coord 1) value)]))

(defn block-coords [coord]
  (let [row-offset (* 3 (int (Math/floor (/ (row-coord coord) 3))))
        col-offset (* 3 (int (Math/floor (/ (col-coord coord) 3))))]
    (increase-row-coord (increase-col-coord (top-left-block) col-offset) row-offset)))

(defn block-values [board coord]
  (let [add-value-to-set (fn [current-set coordinate]
                           (conj current-set (value-at board coordinate)))]
    (reduce add-value-to-set #{} (block-coords coord))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (block-values board coord) (row-values board coord) (col-values board coord))))

(defn all-coords []
  (coord-pairs (range 0 9)))

(defn filled? [board]
  (let [check-if-filled (fn [coord-seq]
                          (cond
                            (empty? coord-seq) true
                            (not (has-value? board (first coord-seq))) false
                            :else (recur (rest coord-seq))))]
    (check-if-filled (all-coords))))

(defn valid-sets [set-seq]
  (= #{all-values} (set set-seq)))

(defn rows [board]
  (let [add-row-values-to-seq (fn [a-seq row]
                               (conj a-seq (row-values board [row 0])))]
    (reduce add-row-values-to-seq [] (range 0 9))))

(defn valid-rows? [board]
  (valid-sets (rows board)))

(defn cols [board]
  (let [add-col-values-to-seq (fn [a-seq col]
                               (conj a-seq (col-values board [0 col])))]
    (reduce add-col-values-to-seq [] (range 0 9))))

(defn valid-cols? [board]
  (valid-sets (cols board)))

(defn all-block-values-helper [board a-seq row col]
  (cond
    (and (>= row 6) (>= col 6)) (conj a-seq (block-values board [row col]))
    (>= col 6) (all-block-values-helper board (conj a-seq (block-values board [row col])) (+ row 3) 0)
    :else (all-block-values-helper board (conj a-seq (block-values board [row col])) row (+ col 3))))

(defn blocks [board]
  (all-block-values-helper board [] 0 0))

(defn valid-blocks? [board]
  (valid-sets (blocks board)))

(defn valid-solution? [board]
  (and (valid-cols? board) (valid-rows? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  nil)

(defn find-empty-point [board]
  nil)

(defn solve [board]
  nil)
