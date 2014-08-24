(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [row _]]
  (set (get board row)))

(defn col-values [board [_ col]]
  (set (map (fn [x] (get x col)) board)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-pairs [[row col]]
  (let [r-min (* 3 (quot row 3))
        c-min (* 3 (quot col 3))
        pairs (for [r (range r-min (+ 3 r-min))
                    c (range c-min (+ 3 c-min))]
                [r c])]
    pairs))

(defn block-values [board coord]
  (set (map (fn [x] (value-at board x)) (block-pairs coord))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (set/union (row-values board coord)
                                          (col-values board coord)
                                          (block-values board coord)))))

(defn filled? [board]
  (let [all-values (set (apply concat board))]
    (not (contains? all-values 0))))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  nil)

(defn cols [board]
  (let [c-range (range (count (first board)))]
    (map (fn [x] (col-values board [0 x])) c-range)))

(defn valid-cols? [board]
  nil)

(defn blocks [board]
  (let [b-range (range 0 (count (first board)) 3)
        pairs   (for [x b-range
                      y b-range]
                  [x y])]
    (map (fn [x] (block-values board x)) pairs)))

(defn valid-blocks? [board]
  nil)

(defn valid-solution? [board]
  nil)

(defn set-value-at [board coord new-value]
  nil)

(defn find-empty-point [board]
  nil)

(defn solve [board]
  nil)
