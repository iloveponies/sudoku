(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(def all-coords
  (for [i (range 9)
        j (range 9)]
    [i j]))

(def all-top-lefts
  (for [i [0 3 6]
        j [0 3 6]]
    [i j]))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [x y]]
  ; rows are arrays within the board
  (set (get board x)))

(defn col-values [board [x y]]
  ; columns consist of nth members of each row
  (set (map (fn [row] (get row y)) board)))

(defn coord-pairs [coord]
  (for [i coord
        j coord]
    [i j]))

(defn top-left-corner [[x y]]
  [(* 3 (int (/ x 3))) (* 3 (int (/ y 3)))])

(defn block-values [board coord]
  (let [[tlx tly] (top-left-corner coord)]
    (set (for [i [tlx (+ 1 tlx) (+ 2 tlx)]
               j [tly (+ 1 tly) (+ 2 tly)]]
           (get-in board [i j])))))           

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{} ;has already value        
    (let [used-in-row (row-values board coord) ;has no value
          used-in-col (col-values board coord)
          used-in-block (block-values board coord)]
      (set/difference all-values (set/union used-in-row used-in-col used-in-block)))))

(defn filled? [board]
  (every? (fn [coord] (has-value? board coord)) all-coords))

(defn rows [board]
  (map (fn [row] (set row)) board))

(defn cols [board]
  (for [i (range 9)]
    (col-values board [0 i])))

(defn blocks [board]
  (map (fn [tl] (block-values board tl)) all-top-lefts))

(defn valid-rows? [board]
  (every? (fn [row] (= row all-values)) (rows board)))

(defn valid-cols? [board]
  (every? (fn [col] (= col all-values)) (cols board)))

(defn valid-blocks? [board]
  (every? (fn [blk] (= blk all-values)) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn empty-points [board]
  (filter (fn [coord] (not (has-value? board coord))) all-coords))

(defn find-empty-point [board]
  (first (empty-points board)))

(defn solve [board]
  (cond 
   (and (filled? board) (valid-solution? board)) [board]
   (and (filled? board) (not (valid-solution? board))) []
   :default (first (for [empty (empty-points board)
                  new-value all-values
                  solved-board (solve (set-value-at board empty new-value))]
              solved-board))))
