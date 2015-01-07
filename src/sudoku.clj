(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})
(def the-empty-val 0)

(def value-at get-in)

(defn is-empty? [board coord]
  (== (value-at board coord) the-empty-val) )
  
(def has-value? (complement is-empty?))

(defn row-values [board [row _]]
  (set (get board row)) )

(defn col-values [board [_ col]]
  (set (reduce (fn [res row] (conj res (get row col))) '() board)) )

(defn coord-pairs
  ([coords]
    (coord-pairs coords coords) )
  ([row_coords col_coords]
    (for [row row_coords
		  col col_coords]
	  [row col] ) ) )

(defn from-m-to-n [m n]
  "Creates the sequence (m m+1 m+2 .... n)"
  (loop [res []
		 m m ]
    (if (> m n)
	    res
	    (recur (conj res m) (inc m)) ) ) )
	
(defn block-pairs [[row col]]
  " Creates a sequence of coordinate pairs
    which all belong to a block that contains
	the coordinate [row col]."
  (let [block-map {0 0 1 0 2 0 3 3 4 3 5 3 6 6 7 6 8 6}
		upper-y   (get block-map row)
		left-x    (get block-map col) ]
	(coord-pairs (from-m-to-n upper-y (+ upper-y 2))
			     (from-m-to-n left-x  (+ left-x  2)) ) ) )	    
	
(defn block-values [board coord]
  (set (reduce (fn [res coord]
                 (conj res (value-at board coord)) )
		       '()
		       (block-pairs coord) ) ) )

(defn valid-values-for [board coord]
  (set/difference
		 all-values
		(set/union
			(row-values board coord)
			(col-values board coord)
			(block-values board coord) ) ) )

(defn member [val col]
  "checks whether val is an element of the collection col.
   Returns the entire tail of the collection beginning with
   the element val if found, false otherwise (just like
   in Common Lisp)"
  (cond
    (empty? col) false
	(= (first col) val) col
	:else (member val (rest col)) ) )
	
(defn filled? [board]
  (not (member the-empty-val (reduce concat board))) )

(defn rows [board]
  (for [row board]
    (set row) ) )

(defn valid-set-collection? [set-collection]
  (reduce (fn [res set] (and res (= set all-values))) true set-collection) ) 
  
(defn valid-rows? [board]
  (valid-set-collection? (rows board)) )

(defn cols [board]
  (for [col (from-m-to-n 0 8)]
    (col-values board [0 col]) ) )

(defn valid-cols? [board]
  (valid-set-collection? (cols board)) )

(defn blocks [board]
  (for [block-center [[1 1] [1 4] [1 7]
					  [4 1] [4 4] [4 7]
					  [7 1] [7 4] [7 7] ]]
	(block-values board block-center) ) )

(defn valid-blocks? [board]
  (valid-set-collection? (blocks board)) )

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)) )

(defn set-value-at [board coord new-value]
  nil)

(defn find-empty-point [board]
  nil)

(defn solve [board]
  nil)
