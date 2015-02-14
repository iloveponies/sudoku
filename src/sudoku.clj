(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[row col] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[row col] coord]
    (set (map #(get % col) board))))

(defn coord-pairs [coords]
  (for [coord1 coords
        coord2 coords]
    [coord1 coord2]))

(defn block-values [board coord]
  (let [get-upper-left-corner 
        (fn [coord]
          (map #(* (int (/ %1 3)) 3) coord))
        [row col] (get-upper-left-corner coord)        
        row-range (range row (+ row 3))
        col-range (range col (+ col 3))
        all-coords (for [x row-range y col-range] [x y])]
    (set (map #(value-at board %) all-coords))))

(defn valid-values-for [board coord]
  (let [used-values 
        (set/union 
         (block-values board coord)
         (col-values board coord)
         (row-values board coord))]
    (if (has-value? board coord)
      #{}
      (set/difference all-values used-values))))

(defn filled? [board]
  (let [filled-helper
        (fn [] (for [x (range 0 9)
                     y (range 0 9)]
                 [x y]))]
    (reduce #(and %1 %2) true (map #(has-value? board %) (filled-helper)))))

(defn rows [board]
  (map #(set %) board))

(defn valid-rows? [board]
  (reduce 
   (fn [acc elem] (and acc elem)) 
   true 
   (map #(= all-values %) (rows board))))

(defn cols [board]
  (let [transposed (vec (apply map vector board))]
    (rows transposed)))

(defn valid-cols? [board]
  (reduce 
   (fn [acc elem] (and acc elem)) 
   true 
   (map #(= all-values %) (cols board))))

(defn blocks [board]
  (let [upper-left-corners 
        (for [x [0 3 6]
              y [0 3 6]]
          [x y])]
    (map #(block-values board %) upper-left-corners)))

(defn valid-blocks? [board]
  (reduce 
   (fn [acc elem] (and acc elem)) 
   true 
   (map #(= all-values %) (blocks board))))

(defn valid-solution? [board]
  (and
   (valid-blocks? board)
   (valid-cols? board)
   (valid-rows? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [all-coords 
        (for [row (range 0 9)
              col (range 0 9)]
          [row col])]
    (loop [coords all-coords]
      (if (not (has-value? board (first coords)))
        (first coords)
        (recur (rest coords))))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      '())
    (let [empty-point (find-empty-point board)]
      (for [valid-value (valid-values-for board empty-point)
            solution (solve (set-value-at board empty-point valid-value))]
        solution))))
