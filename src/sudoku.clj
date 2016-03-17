(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9 })

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  not (== (value-at board coord) 0)))

(defn row-values [board coord]
  (let [[r _] coord](set (get board r))))

(defn col-values [board coord]
  (let [[_ c] coord]
  (set (map (fn [x] (get x c)) board))))

(defn coord-pairs [coords]
  (for [m coords n coords][m n]))

(defn- block-range [[x y]]
  (let [top-left-x (* (quot x 3) 3)
        top-left-y (* (quot y 3) 3)
        bottom-right-x (+ top-left-x 3)
        bottom-right-y (+ top-left-y 3)]
    [top-left-x top-left-y bottom-right-x bottom-right-y]))

(defn block-values [board coord]
  (let [[top-left-x top-left-y bottom-right-x bottom-right-y] (block-range coord)]
    (into #{} (for [x (range top-left-x bottom-right-x) y (range top-left-y bottom-right-y)]
                (value-at board [x y])))))


(defn valid-values-for [board coord]
  (let [row-values (row-values board coord)
        col-values (col-values board coord)
        blk-values (block-values board coord)]
    (if (has-value? board coord)
      #{}
      (set/difference all-values row-values col-values blk-values))))


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
