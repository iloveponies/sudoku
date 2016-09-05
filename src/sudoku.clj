(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
	(get (get board (get coord 0)) (get coord 1))
)

(defn has-value? [board coord]
	(not (== (value-at board coord) 0))
)

(defn row-values [board coord]
	(set (get board (get coord 0)))
)

(defn col-values [board coord]
	(set (map (fn [x] (get x (get coord 1))) board))
)

(defn coord-pairs [coords]
	(for [x coords y coords]
		(vector x y)
	)
)

(defn helper [coords]
	(let [[x y] coords]
		(for [a [x (+ x 1) (+ x 2)] b [y (+ y 1) (+ y 2)]]
			(conj [] a b)
		)
	)
)

(defn block-values [board coord]
	(let [[x y] coord help (fn [x] (value-at board x))]
		(set (map help (helper [(* 3 (int (/ x 3))) (* 3 (int (/ y 3)))])))
	)
)

(defn valid-values-for [board coord]
	(if (not (= 0 (value-at board coord)))
		#{}
		(set/difference (set (range 1 10)) (set/union (row-values board coord) (col-values board coord) (block-values board coord)))	
	)
)

(defn filledhelper [board]
	(let [rows (for [row (range 0 9)] (row-values board [row 1]))]
		(reduce #(set/union %1 %2) rows)
	)
)
(defn filled? [board]
	(not (contains? (filledhelper board) 0))
)

(defn rows [board]
	(for [row (range 0 9)]
		(row-values board [row 1])
	)
)

(defn valid-rows? [board]
	(not (contains? (set (for [row (rows board)] (= row (set (range 1 10))))) false))
)

(defn cols [board]
	(for [col (range 0 9)]
		(col-values board [1 col])
	)
)



(defn valid-cols? [board]
	(not (contains? (set (for [col (cols board)] (= col (set (range 1 10))))) false))
)

(defn blocks [board]
	(for [b [[0 0] [0 3] [0 6] [3 0] [3 3] [3 6] [6 0] [6 3] [6 6]]]
		(block-values board b)
	)
)

(defn valid-blocks? [board]
	(not (contains? (set (for [block (blocks board)] (= block (set (range 1 10))))) false))
)

(defn valid-solution? [board]
	(and (valid-rows? board) (valid-cols? board) (valid-blocks? board))
)

(defn set-value-at [board coord new-value]
	(assoc-in board coord new-value)
)

(defn find-empty-point [board]
	(loop [b board x 0 y 0]
		(cond
			(not (has-value? b [x y])) [x y]
			(< x 8) (recur b (+ x 1) y)
			(< y 8) (recur b 0 (+ y 1))
			:else nil
		)
	)
)

			
(defn solve [board]
	(if (valid-solution? board)
		board
		(let [coord (find-empty-point board) validvals (valid-values-for board coord)]
			(for [validval validvals solution (solve (set-value-at board (find-empty-point board) validval))] 
				solution
			)
		)
	)
)
