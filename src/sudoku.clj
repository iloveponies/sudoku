(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
 (get-in board coord
))

(defn has-value? [board coord]
 (not (= (value-at board coord) 0)
))

(defn row-values [board coord]
 (let [x (get coord 0)]
  (reduce conj #{} (get board x)
)))

(defn get-column [board y]
 (for [x (range 9)] (value-at board [x y])
))

(defn col-values [board coord]
 (let [y (get coord 1)]
  (reduce conj #{} (get-column board y)
)))

(defn coord-pairs-xy [xcoords ycoords]
 (for [x xcoords y ycoords] (vector x y)
))

(defn coord-pairs [coords]
 (coord-pairs-xy coords coords
))

(defn div3 [num]
 (* (int (/ num 3)) 3
))

(defn top-left-block-coord [coord] [(div3 (get coord 0)) (div3 (get coord 1))]
)

(defn get-block [coord]
 (let [c (top-left-block-coord coord)]
  (into []
   (coord-pairs-xy [(get c 0) (+ 1 (get c 0)) (+ 2 (get c 0))] [(get c 1) (+ 1 (get c 1)) (+ 2 (get c 1))]
))))

(defn block-values [board coord]
 (reduce
 	(fn [set coord] (conj set (value-at board coord)));#(value-at board %)
 	#{} 
 	(get-block coord)
))

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn valid-values-for [board coord]
 (if (= (value-at board coord) 0)
  (set/difference all-values
   (set/union
  	(row-values board coord)
  	(col-values board coord)
  	(block-values board coord)
  ))
  #{}
))

(defn filled? [board]
 (not (contains? (into #{} (flatten board)) 0)
))

(defn rows [board]
 (reduce #(conj %1 (set %2)) [] board
))

(defn valid-x? [x]
 (reduce #(and %1 (empty?(set/difference all-values %2))) true x
))

(defn valid-rows? [board]
 (valid-x? (rows board)
))

(defn transpose [board]
  (apply mapv vector board))

(defn cols [board]
 (reduce #(conj %1 (set %2)) [] (transpose board)
))

(defn valid-cols? [board]
 (valid-x? (cols board)
))

(defn blocks [board]
 (reduce #(conj %1 (set %2)) [] (map #(block-values board %) (coord-pairs [0 3 6]))
))

(defn valid-blocks? [board]
 (valid-x? (blocks board)
))

(defn valid-solution? [board]
 (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)
))

(defn set-value-at [board coord new-value]
 (assoc-in board coord new-value
))

(defn find-empty-point [board]
 (first
  (reduce #(if (= (value-at board %2) 0) (conj %1 %2) %1) [] (coord-pairs (range 1 10))
)))

(defn solve [board]
  nil)
