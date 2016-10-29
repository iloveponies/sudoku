(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (into #{} (get board (first coord))))

(defn col-values [board coord]
 (let [[row col] coord func (fn [row] (get row col))]
   (into #{} (map func board))))

(defn coord-pairs [coords]
  (into [] (for [x coords
      y coords]
 [x y])))

(defn top-left-helper [coords]
 (map (fn [x] (* 3 (int (/ x 3)))) coords))

(defn block-values [board coord]
  (let [[row col] (top-left-helper coord)
        row-travel (range row (+ 3 row))
        col-travel (range col (+ 3 col))]
    (into #{}(for [x row-travel
          y col-travel]
      (get-in board [x y])))))


(defn valid-values-for [board coord]
  (if (not(zero? (get-in board coord)))
    #{}
    (set/difference all-values (set/union
   (row-values board coord) (col-values board coord) (block-values board coord)))))


(defn filled? [board]
  (let [values (set (reduce concat board))]
    (if (contains? values 0)
      false
      true)))

(defn rows [board]
  (into [](for [row board]
    (set row))))


(defn valid-rows? [board]
  (let [valid (fn [values] (= all-values values))]
    (every? valid (rows board))))


(defn cols [board]
  (let [length (count (first board))]
    (into [] (for [i (range 0 length)]
       (col-values board [0 i])))))

(defn valid-cols? [board]
  (let [valid (fn [values] (= all-values values))]
    (every? valid (cols board))))

(defn blocks [board]
  (let [get-blocks (coord-pairs [0 3 6])
        values (fn [coord] (block-values board coord))]
    (into [] (map values get-blocks))))

 (defn valid-blocks? [board]
    (let [valid (fn [values] (= all-values values))]
    (every? valid (blocks board))))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [ind (range 0 9)
        coords (coord-pairs ind)
        zero (fn [val] (not (has-value? board val)))]
    (first(filter zero coords))))

(defn solve-helper [current-board]
  (let [empty-p (find-empty-point current-board)]
  (if (nil? empty-p)
    (if (valid-solution? current-board)
      [current-board]
      [])
    (for [value (valid-values-for current-board empty-p)
          solution (solve-helper (set-value-at current-board empty-p value))]
      solution))))


(defn solve [board]
  (first (solve-helper board)))
