(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board [row _]]
  (set (get board row)))

(defn col-values [board [_ col]]
  (set
    (map
      (fn [x] (value-at board [x col]))
      (range 0 9))
    )
  )

(defn used-values [board]
  (reduce clojure.set/union #{} (for [row (range 0 9)]
                                  (row-values board [row 0])
                                  ))
  )

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn top-left-corner [[row col]]
  (let [newRow (* (int (Math/floor (/ row 3))) 3)
        newCol (* (int (Math/floor (/ col 3))) 3)]
    [newRow newCol])
  )

(defn block-values [board [row col]]
  (let [[topLeftRow topLeftCol] (top-left-corner [row col])
        rows (range topLeftRow (+ topLeftRow 3))
        cols (range topLeftCol (+ topLeftCol 3))
        ]
    (set (for [r rows
               c cols]
           (value-at board [r c])
           ))
    )
  )

(defn valid-values-for [board coord]
  (let [blockvalues (block-values board coord)
        rowvalues (row-values board coord)
        colvalues (col-values board coord)
        usedvalues (clojure.set/union blockvalues rowvalues colvalues)
        ]
    (cond
      (has-value? board coord) #{}
      :else (clojure.set/difference all-values usedvalues)
      )
    )
  )

(defn filled? [board]
  (not (contains? (used-values board) 0))
  )

(defn rows [board]
  (for [row (range 0 9)]
    (row-values board [row 0])
    ))

(defn valid-rows? [board]
  (let [myrows (rows board)]
    (every? (fn [x] (= x all-values)) myrows)
    ))

(defn cols [board]
  (for [col (range 0 9)]
    (col-values board [0 col])
    ))

(defn valid-cols? [board]
  (let [mycols (cols board)]
    (every? (fn [x] (= x all-values)) mycols)
    ))

(defn blocks [board]
  (for [row (range 0 9 3)
        col (range 0 9 3)]
    (block-values board [row col])
    ))

(defn valid-blocks? [board]
  (let [myblocks (blocks board)]
    (every? (fn [x] (= x all-values)) myblocks)
    ))

(defn valid-solution? [board]
  (and
    (valid-rows? board)
    (valid-cols? board)
    (valid-blocks? board)
    )
  )

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value)
  )

(defn find-empty-point [board]
  (first (for [row (range 0 9)
               col (range 0 9)
               :let [coord (if (has-value? board [row col]) nil [row col])]
               :when (boolean coord)]
           coord
           ))
  )

;Recap of backtracking:
;
;check if you are at the end
;if so, is the solution valid?
;if not, return an empty sequence
;otherwise return [solution]
;if not
;select an empty location
;try solving with each valid value for that location

(defn solve [board]
  (if (valid-solution? board)
    board
    (let [empty-point (find-empty-point board)
          valid-values (valid-values-for board empty-point)]
      (for [current-value valid-values
            solution (solve (set-value-at board empty-point current-value))]
        solution))))
