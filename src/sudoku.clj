(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (all-values (value-at board coord)))

(defn row-values [board [y x]]
  (set (get board y)))

(defn col-values [board [y x]]
  (set
    (for [i (range 9)]
      (value-at board [i x]))))

(defn coord-pairs [coords]
  (for [i coords
        j coords]
    [i j]))

(defn block-values [board [y x]]
  (set (map #(value-at board %) (map (fn [[cy cx]] [(+ (- y (mod y 3)) cy) (+ (- x (mod x 3)) cx)]) (coord-pairs (range 3))))))

(defn valid-values-for [board coord]
  (if (has-value? board coord) 
    #{} 
    (clojure.set/difference 
      all-values 
      (row-values board coord) 
      (col-values board coord) 
      (block-values board coord))))

(defn filled? [board]
  (not (contains? (set (reduce concat board)) 0)))

(defn rows [board]
  (for [i (range 9)]
    (row-values board [i 0])))

(defn valid-rows? [board]
  (every? identity (map #(= all-values %) (rows board))))

(defn cols [board]
  (for [i (range 9)]
    (col-values board [0 i])))

(defn valid-cols? [board]
  (every? identity (map #(= all-values %) (rows board))))

(defn blocks [board]
  (map #(block-values board %) (map (fn [[y x]] [(* y 3) (* x 3)]) (coord-pairs (range 3)))))

(defn valid-blocks? [board]
  (every? identity (map #(= all-values %) (rows board))))

(defn valid-solution? [board]
  (and (valid-cols? board) (valid-rows? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first
    (filter 
      #(= (value-at board %) 0) 
      (for [i (range 9) 
            j (range 9)] 
        [j i]))))

(defn solve [board]
  (if (valid-solution? board)
    board
    (let [empty-point (find-empty-point board)
          possible-values (valid-values-for board empty-point)]
      (first 
        (filter 
          (complement nil?) 
          (for [value possible-values]
            (solve (set-value-at board empty-point value))))))))
