(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

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

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))


(defn row-values [board coord]
  (into #{} (map (fn [x] (value-at board [(first coord) x]))  (range 0 9)  ))
  )

(defn col-values [board coord]
  (into #{} (map (fn [x] (value-at sudoku-board [x (second coord)]))  (range 0 9)  )))



(defn coord-pairs [coords]

  (loop
    [res [] notused coords]

    (if (empty? notused)
      res
      (recur (concat res (for
                           [x coords]
                           [(first notused) x]
                           )) (rest notused)
      )

    )

  )
)

(defn top-left [coord]

  [
   (* 3 (int (/ (first coord) 3)))
    ( * 3 (int (/ (second coord) 3)))
   ]
  )

(defn block-values [board coord]

  (let

    [tl (top-left coord)
     c (coord-pairs [0 1 2])
     ]

    (into #{} (map (fn [q] (value-at board q))
    (for [tmp c]

      (map + tmp tl)
      )))

    )

  )

(defn valid-values-for [board coord]

  (if (has-value? board coord)
    #{}
    (set/difference all-values (block-values board coord) (col-values board coord) (row-values board coord))
    )

  )

(defn filled? [board]
  (not (contains? (into #{} (apply concat (map (fn [k] (row-values board [k 0])) (range 0 9)))) 0)))


(defn rows [board]
  (map (fn [k] (row-values board [k 0])) (range 0 9)))


(defn valid-rows? [board]
  nil)

(defn cols [board]
  (map (fn [k] (col-values board [0 k])) (range 0 9)))

(defn valid-cols? [board]
  nil)

(defn blocks [board]
  (for [k (range 0 9)]
    nil
    )
  )

(defn valid-blocks? [board]
  nil)

(defn valid-solution? [board]
  nil)

(defn set-value-at [board coord new-value]
  nil)

(defn find-empty-point [board]
  nil)

(defn solve [board]
  nil)
