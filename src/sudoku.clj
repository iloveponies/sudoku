(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coords]
  (get-in board coords))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [coord _]]
  (set (get board coord)))

(defn col-values [board [_ coord]]
  (set (reduce #(conj %1 (get %2 coord)) [] board)))

(defn coord-pairs [a-seq]
  (for [a a-seq
        b a-seq]
    [a b]))

(defn corner [coord]
  (let [[x y] coord
        f (fn [a] (* (int (/ a 3)) 3))]
  [(f x) (f y)]))


(defn block-values2 [board coord]
  (let [[cx cy] (corner coord)]
    (reduce #(conj %1 (value-at board %2))
       #{}
       (map
         (fn [[a b]] [(+ cx a) (+ cy b)])
         (coord-pairs [0 1 2])))))

(defn block-values [board coord]
  (let [[cx cy] (let [[x y] coord f (fn [a] (* (int (/ a 3)) 3))] [(f x) (f y)])]
    (reduce #(conj %1 (value-at board %2)) #{} (map (fn [[a b]] [(+ cx a) (+ cy b)])(coord-pairs [0 1 2])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
  (set/difference all-values (block-values board coord) (row-values board coord) (col-values board coord))))

(defn filled? [board]
  (not (contains? (set (apply set/union board)) 0)))

(defn rows [board]
  (map set board))

(defn valid-sets [sets]
  (every? identity (map (fn [x] (empty? (set/difference all-values x))) sets)))

(defn valid-rows? [board]
  (valid-sets (rows board)))

(defn cols [board]
  (map set (for [a [0 1 2 3 4 5 6 7 8]]
             (col-values board [0 a]))))

(defn valid-cols? [board]
  (valid-sets (cols board)))

(defn blocks [board]
  (map set (for [coord (coord-pairs [0 3 6])]
             (block-values board coord))))

(defn valid-blocks? [board]
  (valid-sets (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [x 0
         y 0]
    (if (and (> x 8) (> y 8)) [x y]
      (if (> x 8) (recur 0 (+ y 1))
        (if (has-value? board [x y])
          (recur (+ x 1) y)
          [x y])))))

(defn solve-helper [current]
  (if (filled? current)
    (if (valid-solution? current)
      [current]
      [])
    (let [point (find-empty-point current)]
      (for [elem (valid-values-for current point)
            hoho (solve-helper (set-value-at current point elem))]
        hoho))))

(defn solve [board]
  (first (solve-helper board)))
