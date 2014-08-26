(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def board-size 9)
(def block-size 3)
(def blocks (/ board-size block-size))

(def all-values (into #{} (range 1 (inc board-size))))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [row _]]
  (into #{}
        (for [col (range board-size)]
          (value-at board [row col]))))

(defn col-values [board [_ col]]
  (into #{}
        (for [row (range board-size)]
          (value-at board [row col]))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-start [[row col]]
  [(* block-size (quot row block-size))
   (* block-size (quot col block-size))])

(defn block-values [board [row col]]
  (let [[row-start col-start] (block-start [row col])
        row-end (+ row-start block-size)
        col-end (+ col-start block-size)]
    (into #{}
          (for [brow (range row-start row-end)
                bcol (range col-start col-end)]
            (value-at board [brow bcol])))))

(defn valid-values-for [board coord]
  (if-not (zero? (value-at board coord))
    #{}
    (-> all-values
        (set/difference (row-values board coord))
        (set/difference (col-values board coord))
        (set/difference (block-values board coord)))))

(def all-coords
  (for [row (range board-size)
        col (range board-size)]
    [row col]))

(defn all-entries [board]
    (map #(value-at board %) all-coords))

(defn filled? [board]
  (not-any?
   zero?
   (all-entries board)))

(defn all-valid-houses? [houses]
  (every? #(= all-values %) houses))

(defn rows [board]
  (for [row (range board-size)]
    (row-values board [row 0])))

(defn valid-rows? [board]
  (all-valid-houses? (rows board)))

(defn cols [board]
  (for [col (range board-size)]
    (col-values board [0 col])))

(defn valid-cols? [board]
  (all-valid-houses? (cols board)))

(defn blocks [board]
  (for [row [0 3 6]
        col [0 3 6]]
    (block-values board [row col])))

(defn valid-blocks? [board]
  (all-valid-houses? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn empty-points [board]
  (filter #(zero? (value-at board %)) all-coords))

(defn find-empty-point [board]
  (first (empty-points board)))

(defn solve [board]
  nil)
