(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord)
  )

(defn has-value? [board coord]
  (not (= 0 (value-at board coord)))
  )

(defn row-values [board coord]
  (let [[row col] coord] 
    (apply sorted-set (get board row))
  ))

(defn col-values [board coord]
  (let [[row col] coord] 
    (apply sorted-set (map #(get % col) board))
    ))

(defn coord-pairs [coords]
  (for [r coords
        c coords]
    [r c]
    ))

(defn block-pairs [row-range col-range]
  (for [r row-range
        c col-range]
    [r c]
    ))

(defn find-block-upleft-corner [coord]
  (let [[row col] coord] 
    (mapv #(* 3 (quot % 3)) coord)
    ))

(defn block-values [board coord]
  (let [[r c] coord
        pairs-in-block (apply block-pairs (mapv #(range % (+ 3 %)) (find-block-upleft-corner coord)))
        ]
    (set
      (map #(value-at board %) pairs-in-block)
      )
    ))

(defn valid-values-for [board coord]
  (if (not (= 0 (value-at board coord)))
    #{}
    (set/difference 
      (set/difference 
        (set/difference all-values (block-values board coord)) 
        (col-values board coord)
        )
      (row-values board coord)
      )
    ))

(defn filled? [board]
  (not (contains? (set (reduce concat board)) 0))
  )

(defn rows [board]
  (map set board)
  )

(defn cols [board]
  (map set
    (for [col (range 0 9)] 
      (map #(get % col) board)
      )
       )
  )

(defn blocks [board]
  (map #(block-values board %) (coord-pairs [0 3 6]))
  )

(defn valid-9-set? [row-col-block-set]
  (and (= 9 (count row-col-block-set))
       (not (contains? row-col-block-set 0))
       )
  )

(defn valid-rows? [board]
  (every? valid-9-set? (rows board))
  )

(defn valid-cols? [board]
  (every? valid-9-set? (cols board))
  )

(defn valid-blocks? [board]
  (every? valid-9-set? (blocks board))
  )

(defn valid-solution? [board]
  (and
    (valid-rows? board)
    (valid-cols? board)
    (valid-blocks? board)
    )
  )

(defn set-value-at [board coord new-value]
  (let [[row col] coord] 
    (assoc-in board [row col] new-value)
    ))

(defn find-empty-point [board]
  (let [ col-of-first-zero-in-row (mapv #(.indexOf % 0) board)
         first-row-w-zero (.indexOf (mapv #(= % -1) col-of-first-zero-in-row) false) 
         row first-row-w-zero
         col (get col-of-first-zero-in-row row) ]
    [row col]
    )
  )

(defn sum [a-seq]
  (reduce + a-seq))

(defn subset-sum-helper [a-set current-set target]
  (if (= (sum current-set) target)
    [current-set]
    (let [remaining (clojure.set/difference a-set current-set)]
      (for [elem remaining
            solution (subset-sum-helper a-set
                                        (conj current-set elem)
                                        target)]
        solution))))

(defn subset-sum [a-set target]
  (subset-sum-helper a-set #{} target)
)

(defn solve-sudoku-helper [orig-board solve-board]
  (if (and 
        (filled? solve-board)
        (valid-solution? solve-board)
        ) 
    [solve-board]

    (let [fill-square (find-empty-point solve-board)
          fill-choices (valid-values-for solve-board fill-square) ]

      (for [elem fill-choices
            solution (solve-sudoku-helper solve-board (set-value-at solve-board fill-square elem)) ]

        solution)
      )
    )
  )

(defn solve [board]
  (first 
    (solve-sudoku-helper board board)
    )
  )

