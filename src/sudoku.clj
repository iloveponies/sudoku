(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (cond (== 0 (value-at board coord)) false
        :else true
  )
  )

(defn row-values [board coord]
  (set (get board (get coord 0)))
  )

(defn col-values [board coord]
  (set (for [row (range 0 9)]
    (get-in board (vector row (get coord 1))))
  )
)

(defn coord-pairs [coords]
   (vec (for [x coords y coords] [x y]))
  )

(defn top-left [[x y]]
	 (let [top-x (* (quot x 3) 3)
	  top-y (* (quot y 3) 3)]
	 [top-x top-y])
  )

(defn block-values [board coords]

  (let [x (get (top-left coords) 0) y (get (top-left coords) 1)]
   (set (for [x (range x (+ x 3))
         y (range y (+ y 3))]
          (get-in board [x y]))
   )
    )
  )

(defn valid-values-for [board coord]
  (cond (has-value? board coord) #{}
      :else (set/difference all-values (row-values board coord) (col-values board coord) (block-values board coord)
            )
        )
  )
(defn board-values [board]
  (for [x (range  0 9)
         y (range  0 9)]
          (get-in board [x y]))
  )
(defn filled? [board]
  (cond (contains? (set (board-values board)) 0) false
        :else true
        )
  )

(defn rows [board]
  (into (vector) (map set board))
  )

(defn valid-rows? [board]
   (cond  (= (set (rows board)) (into #{} (vector all-values)))   true
        :else false
        )

)
(defn cols [board]
  (into [] (map set (apply map list board))))

(defn valid-cols? [board]
    (cond  (= (set (cols board)) (into #{} (vector all-values)))   true
        :else false
        )

  )

(defn blocks [board]
(into (vector)
      (for [x (range 0 9 3)
	       y (range 0 9 3)]
	   (block-values board [x y]))
      )
  )

(defn valid-blocks? [board]
   (cond  (= (set (blocks board)) (into #{} (vector all-values)))   true
        :else false
        )
  )

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board))
  )

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value)
  )

(defn find-empty-point [board]
  (first (filter #(zero? (value-at board %)) (coord-pairs (range 0 9)))
  )
)

(defn solve-helper [board]
	(if (filled? board); if at end
	   (if (valid-solution? board) ; if valid board
       [board]
       []
       )
	   (let [empty-point (find-empty-point board)]; find first empty point
	     (for [valid-value (valid-values-for board empty-point); find a valid value for empty point
	           updated-board (solve-helper (set-value-at board empty-point valid-value))]; update the board
	           updated-board
         )
       )
     )
  )

(defn solve [board]
   (first (solve-helper board)) ; return result as board
  )
