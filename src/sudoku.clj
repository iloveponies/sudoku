(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[r _] coord](set (get board r))))

(defn col-values [board coord]
  (let [[_ c] coord]
  (set (map (fn [x] (get x c)) board))))

(defn coord-pairs [coords]
  (for [m coords n coords][m n]))

(defn block-values [board coord]
  (let [[r c] coord
   tlc-m (* (quot r 3) 3)
   tlc-n (* (quot c 3) 3)
   br-m (vec (range tlc-m (+ tlc-m 3)))
   br-n (vec (range tlc-n (+ tlc-n 3)))]
  (set (for [m br-m
             n br-n]
  (get-in board [m n])))))

(defn valid-values-for [board coord]
  (let [v (set (range 1 10))
   rv (row-values board coord)
   cv (col-values board coord)
   bv (block-values board coord)
   rsv (set/union rv cv bv)]
  (if (has-value? board coord)
   #{}(set/difference v rsv))))

(defn filled? [board]
  (not (some zero? (flatten board))))

(defn rows [board]
  (map #(row-values board [% 0])
  (range 0 9)))

(defn valid-rows? [board]
  nil)

(defn cols [board]
   (map #(col-values board [0 %])
   (range 0 9)))

(defn valid-cols? [board]
  nil)

(defn blocks [board]
  (map #(block-values board %)
  (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  nil)

(defn valid-solution? [board]
  nil)

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter #(zero? (value-at board %))
       (coord-pairs (range 0 9)))))

(defn solve [board]
  nil)
