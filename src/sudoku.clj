(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord)
)

(defn has-value? [board coord]
  (not= 0 (value-at board coord))
)

(defn row-values [board [row _]]
  (set (get board row))
)

(defn col-values [board [_ col]]
  (set (for [row board] (get row col)))
)

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]
  )
)

(defn top-left-coords [coords]
  (let [[row col] coords]
    [(- row (mod row 3)) (- col (mod col 3))]
  )
)

(defn block-values [board coord]
  (let
    [[block-y block-x] (top-left-coords coord)]
    (set
      (for [row (range block-y (+ 3 block-y))
            col (range block-x (+ 3 block-x))]
        (value-at board [row col])
      )
    )
  )
)

(defn valid-values-for [board coord]
  (if
    (has-value? board coord)
    #{}
    (set/difference all-values
      (row-values board coord)
      (col-values board coord)
      (block-values board coord)
    )
  )
)

(defn filled? [board]
  (not (contains? (set (apply concat board)) 0))
)

(defn rows [board]
  (for [row board]
    (set row)
  )
)

(defn valid-rows? [board]
  (every? #(= %1 all-values) (rows board))
)

(defn cols [board]
  (loop [board board
         result []]
    (if
      (some empty? board)
      result
      (recur (map rest board) (conj result (set (map first board))))
    )
  )
)

(defn valid-cols? [board]
  (every? #(= %1 all-values) (cols board))
)

(defn block [board-seq [row col]]
  (let
    [row row
     col col
     start (drop (+ (* row 27) (* col 3)) board-seq)]
    (set
      (apply concat (take 3 (partition 3 9 start)))
    )
  )
)

(defn blocks [board]
  (let
    [board-seq (apply concat board)]
    (for [row (range 0 3) col (range 0 3)]
      (block board-seq [row col])
    )
  )
)

(defn valid-blocks? [board]
  (every? #(= %1 all-values) (blocks board))
)

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board))
)

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value)
)

(defn find-empty-col [board row]
  (loop [col 0]
    (let
      [has-value (has-value? board [row col])]
      (cond
        (> col 8) nil
        (= has-value false) col
        :else (recur (inc col))
      )
    )
  )
)

(defn find-empty-point [board]
  (loop [row 0]
    (let
      [col (find-empty-col board row)]
      (cond
        (> row 8) nil
        (not (nil? col)) [row, col]
        :else (recur (inc row))
      )
    )
  )
)

(defn solve [board]
  (if
    (nil? (find-empty-point board))
    (if (valid-solution? board)
      board
      nil
    )
    (let
      [empty-point (find-empty-point board)
       valid-values (valid-values-for board empty-point)
       res (for [value valid-values]
             (solve (set-value-at board empty-point value))
           )
      ]
      (if (empty? res)
        []
        (first (filter not-empty res))
      )
    )
  )
)
