(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (zero? (value-at board coord))
    false
    true)
  )

(defn row-values [board coord]
  (set (get board (first coord)))
  )

(defn col-values [board coord]
  ;; retry with for aye ??
  (let [col (last coord)]
    (loop [row 0
           a-set #{}]
      (if (= row 9)
        a-set
        (recur (inc row)
               (conj a-set (value-at board (vector row col)))))))
  )

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    (vector row col))
  )

(defn block-values [board coord]
  ;; For a given coordinates, get the set of values in the block this
  ;; coordinate is part of
  (let [x-coord (first coord)
        y-coord (last coord)
        top-left-x (* (quot x-coord 3) 3)
        top-left-y (* (quot y-coord 3) 3)
        block-pairs (for [row (range top-left-x (+ top-left-x 3))
                          col (range top-left-y (+ top-left-y 3))]
                      (vector row col))]
    (reduce #(conj %1 (value-at board %2)) #{} block-pairs))
  )

(defn valid-values-for [board coord]
  
  )

(defn filled? [board]
  nil)

(defn rows [board]
  nil)

(defn valid-rows? [board]
  nil)

(defn cols [board]
  nil)

(defn valid-cols? [board]
  nil)

(defn blocks [board]
  nil)

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
