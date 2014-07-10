(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (let [value (value-at board coord)
		blank (zero? value)]
	(not blank)))

(defn row-values [board coord]
  (let [[y x] coord
		row (get board y)]
	(set row)))

(defn col-values [board coord]
  (let [[_, x] coord
		get-at-col (fn [row] (get row x))
		col (map get-at-col board)]
	(set col)))

(defn coord-pairs [coords]
  (for [row coords
		col coords]
	[row col]))

(defn origin-of-block [coord]
  (let [bound-n (fn [n]
				  (cond (< n 3) 0
						(< n 6) 3
						(< n 9) 6))]
	(map bound-n coord)))

(defn block-values [board coord]
  (let [[oy ox] (origin-of-block coord)]
	(set
	 (for [y (range oy (+ oy 3))
		   x (range ox (+ ox 3))]
	   (value-at board [y x])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
	#{}
	(let [row (row-values board coord)
		  col (col-values board coord)
		  block (block-values board coord)
		  all (set/union row col block)]
	  (set/difference all-values all))))

(defn all-board-vals [board]
  (reduce concat board))

(defn has-blanks [a-set]
  (contains? a-set 0))

(defn filled? [board]
  (let [all (apply vector (all-board-vals board))
		present (set all)]
	(not (has-blanks present))))

(defn valid-regions? [board regions]
  (let [valid-region? (fn [region] (= all-values region))
		tests (map valid-region? regions)]
	(every? identity tests)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (valid-regions? board (rows board)))

(defn cols [board]
  (for [x (range 0 9)]
	(set (for [y (range 0 9)]
		   (value-at board [y x])))))

(defn valid-cols? [board]
  (valid-regions? board (cols board)))

(defn blocks [board]
  (apply vector
		 (for [y [0 3 6]
			   x [0 3 6]]
		   (block-values board [y x]))))

(defn valid-blocks? [board]
  (valid-regions? board (blocks board)))

(defn valid-solution? [board]
  (let [r (valid-rows? board)
		c (valid-cols? board)
		b (valid-blocks? board)]
	(and r c b)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [helper (fn [y x]
				 (let [end-of-cols? (< 8 x)
					   end-of-rows? (< 8 y)]
				   (cond (and end-of-cols? end-of-rows?) nil
						 end-of-cols? (recur (inc y) 0)
						 (has-value? board [y x]) (recur y (inc x))
						 :else [y x])))]
	(helper 0 0)))

(defn solve-helper [board]
  (if (filled? board)
	(if (valid-solution? board)
	  [board]
	  [])
	(let [empty-loc (find-empty-point board)]
	  (for [value (valid-values-for board empty-loc)
			sol (solve-helper (assoc-in board
										empty-loc
										value) )]
		sol))))

(defn solve [board]
  (first (solve-helper board)))








