(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board coord]
  (let [[y] coord]
    (set (get-in board [y]))))

(defn col-values [board coord]
  (let [[y x] coord]
    (set (map (fn [a] (get-in board [a x]) ) (range 0 9)))))

(defn coord-pairs [coords]
  (let [pairs (for [n coords
                    m coords]
                [n m])]
    (reduce conj [] pairs)))

(defn atcorner [board coord]
  (let [[y x] coord
        assigner (fn [a]
                   (cond
                     (>= a 6) 6
                     (>= a 3) 3
                     :else 0))]
    [(assigner y) (assigner x)]))

(defn block-coord-pairs [ycoords xcoords]
  (let [pairs (for [n xcoords m ycoords]
                [m n])]
    (reduce conj [] pairs)))

(defn block-values [board coord]
  (let [cornercoords (atcorner board coord)
        [y x] cornercoords
        block-coords (block-coord-pairs
                       (range y (+ 3 y))
                       (range x (+ 3 x)))]
    (set (map (fn [a] (value-at board a)) block-coords))))

(defn valid-values-for [board coord]
  nil)

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
