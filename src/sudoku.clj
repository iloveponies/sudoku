(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  ((complement zero?) (value-at board coord)))

(defn row-values [board coord]
  (let [[x1 x2 x3 x4 x5 x6 x7 x8 x9] (get board (get coord 0)) ]
    (conj #{} x1 x2 x3 x4 x5 x6 x7 x8 x9 )))

(defn col-values [board coord]
  (loop [x 0 colset #{}]
    (if (> x 8)
      colset
      (recur (inc x) (conj colset (value-at board [x (get coord 1)]))))))

(defn coord-pairs [coords]
  (for [first coords second coords]
    [first second]))

(defn block-helper [coords]
  (let [[x y] coords]
  [(- x (mod x 3)) (- y (mod y 3))]))

(defn block-values [board coord]
  (let [[x y] (block-helper coord)]
    (set (for [coordinate (coord-pairs [x (+ x 1) (+ x 2)])]
       (value-at board coordinate)))))

(defn valid-values-for [board coord]
  (if (zero? (value-at board coord))
    (set/difference (set/difference (set/difference #{1 2 3 4 5 6 7 8 9}
                                                    (block-values board coord))
                                    (row-values board coord))
                    (col-values board coord))
    #{}))


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
