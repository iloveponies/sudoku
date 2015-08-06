(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(comment
  (def sudoku-board
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

(def solved-board
  (board [[5 3 4 6 7 8 9 1 2]
          [6 7 2 1 9 5 3 4 8]
          [1 9 8 3 4 2 5 6 7]
          [8 5 9 7 6 1 4 2 3]
          [4 2 6 8 5 3 7 9 1]
          [7 1 3 9 2 4 8 5 6]
          [9 6 1 5 3 7 2 8 4]
          [2 8 7 4 1 9 6 3 5]
          [3 4 5 2 8 6 1 7 9]]))
)

(def solved-board-dev
  (board [[5 3 4 6 7 8 9 1 2]
          [6 7 2 1 9 5 3 4 8]
          [1 9 8 3 4 2 5 6 7]
          [8 5 9 7 6 1 4 2 3]
          [4 2 6 8 5 3 7 9 1]
          [7 1 3 9 2 4 8 5 6]
          [9 6 1 5 3 7 2 8 4]
          [2 8 7 4 1 9 6 3 5]
          [3 4 5 2 8 6 1 7 9]]))

(def sudoku-board-dev
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

(defn value-at [board coord]
    (get-in board coord)
)

;;(value-at sudoku-board [0 1]) ;=> 3
;;(value-at sudoku-board [0 0]) ;=> 5

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

;;(has-value? sudoku-board [0 0]) ;=> true
;;(has-value? sudoku-board [0 2]) ;=> false

(defn row-values [board [row _]]
  (set (get board row)))


;;(row-values sudoku-board [0 2]) ;=> #{0 5 3 7}
;;(row-values sudoku-board [3 2]) ;=> #{0 8 6 3}

(defn col-values [board [_ col]]
  (set (map
        (fn [row] (get row col))
        board))
)

;;(col-values sudoku-board [0 2]) ;=> #{0 8}
;;(col-values sudoku-board [4 8]) ;=> #{3 1 6 0 5 9}

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(coord-pairs (range 0 9))

;;(coord-pairs [0 1 2])

(defn top-left [[row col]]
  [(- row (mod row 3)) (- col (mod col 3))])

;;(top-left [3 6])

(defn block-values [board coord]
  (let [tl (top-left coord)
        t (first tl)
        l (first (next tl))
        block-coords (for [row (range t (+ t 3))
                           col (range l (+ l 3))]
                       [row col])
        ]
    (set (map (fn [coords] (value-at board coords)) block-coords))
    ))

;;(block-values sudoku-board [0 2])
;;(block-values sudoku-board [4 5])

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                  (set/union (block-values board coord)
                             (col-values board coord)
                             (row-values board coord)))))

;;(block-values sudoku-board-dev [0 0])
;;(row-values sudoku-board-dev [0 0])
;;(col-values sudoku-board-dev [0 0])

;;(valid-values-for sudoku-board-dev [0 0]) ;=> #{}
;;(valid-values-for sudoku-board-dev [0 2]) ;=> #{1 2 4})

;;(contains? (set (apply set/union [[1 2 3] [2 3 4] [4 5 6]])) 1)

(defn filled? [board]
  (not (contains? (set (apply set/union board)) 0)))

;;(filled? sudoku-board) ;=> false
;;(filled? solved-board) ;=> true

(defn rows [board]
  (map (fn [row] (set row)) board)
  )

(rows sudoku-board-dev)

(defn check-all-values-present [values]
            (= #{}
               (set/difference
                (set (range 1 10))
                values)))

(defn valid-rows? [board]
  (every? check-all-values-present (rows board)))

(valid-rows? sudoku-board-dev)
(valid-rows? solved-board-dev)

(defn cols [board]
  (for [col (range 0 9)] (col-values board [0 col])))

(defn valid-cols? [board]
  (every? check-all-values-present (cols board)))

(valid-cols? sudoku-board-dev)
(valid-cols? solved-board-dev)

(defn blocks [board]
  (let [vals [0 3 6]]
    (for [row vals col vals] (block-values board [row col]))))

(blocks sudoku-board-dev)

(defn valid-blocks? [board]
  (every? check-all-values-present (blocks board)))

(valid-blocks? sudoku-board-dev)
(valid-blocks? solved-board-dev)

(defn valid-solution? [board]
  (and
   (valid-rows? board)
   (valid-cols? board)
   (valid-blocks? board)))

(valid-solution? sudoku-board-dev)
(valid-solution? solved-board-dev)

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point
  ([board]
     (find-empty-point board (coord-pairs (range 0 9))))
  ([board [first-coord-pair & remaining-coord-pairs]]
   (cond
     (not (has-value? board first-coord-pair)) first-coord-pair
     (empty? remaining-coord-pairs) nil
     :else (find-empty-point board remaining-coord-pairs))))

(find-empty-point sudoku-board-dev)
(find-empty-point solved-board-dev)

(defn solve [board]
  (let [empty-point (find-empty-point board)]
    (if (nil? (find-empty-point board))
      (if (valid-solution? board)
        board
        [])
      (let [valid-values (valid-values-for board empty-point)]
        (for [value valid-values
              new-board (solve (set-value-at board empty-point value))]
          new-board)))))

(solve sudoku-board-dev)
(find-empty-point sudoku-board-dev)
(valid-values-for sudoku-board-dev [0 2])
(valid-solution? (solve (set-value-at sudoku-board-dev [0 2] 4)))

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

(subset-sum-helper #{13 1 2 3 4 5} #{} 23)

(defn subset-sum [a-set target]
  (subset-sum-helper a-set #{} target))

(class #{})
(class {})
(class [])
(class '())

