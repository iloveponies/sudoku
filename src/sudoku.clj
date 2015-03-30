(ns sudoku
  (:require [clojure.set :as set]))

(def all-values #{1 2 3 4 5 6 7 8 9})

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (> (value-at board coord) 0))

(defn row-values [board coord]
  (let [[row column] coord]
    (set (get board row))
    )
  )

(defn transpose [m]
  (apply mapv vector m)
  )

(defn col-values [board coord]
  (let [[x y] coord]
  (row-values (transpose board) [y x])
    ))

(defn coord-pairs [coords]
  (for [x coords y coords]
      [x y]
    ))

(defn top-left-block [coord]
  (map (fn [x] (* 3 (int (/ x 3))) ) coord)
  )

(defn block-values [board coord]
  (set (let [[bx by] (top-left-block coord)]
    (for [i [0 1 2] j [0 1 2]]
      (value-at board [(+ bx i) (+ by j)] )
      )
    ))
  )

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{} (set/difference all-values (set/union (block-values board coord) (col-values board coord) (row-values board coord)))
                  ))

(defn filled? [board]
  (not (contains? (set (flatten board)) 0)))

(defn rows [board]
  (map (fn [x] (set x)) board))

(defn valid-rows? [board]
  (== (reduce + (map (fn [x] (count (set/difference all-values x))) board)) 0)
  )

(defn cols [board]
  (rows (transpose board)))

(defn valid-cols? [board]
  (valid-rows? (transpose board)))

(defn blocks [board]
  (for [i [0 3 6] j [0 3 6]]
    (block-values board [i j])
    ))

(defn valid-blocks? [board]
  (== (reduce + (map (fn [x] (count (set/difference all-values x))) board)) 0))

(defn valid-solution? [board]
  (and (valid-cols? board) (valid-rows? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn first-0 [s]
  (let [[x y a] (first s)]
    (if (== a 0) [x y] (first-0 (rest s)))
    )
  )

(defn find-empty-point [board]
  (first-0 (map (fn [v] = (let [[x y]  v] [x y (value-at board [x y])]) ) (coord-pairs (range 9)))))

(defn rec-solve [board]
  (if (filled? board)
    (if (valid-solution? board) [board] [])
    (let [coord (find-empty-point board)]
      (for [v (valid-values-for board coord) solution (rec-solve (set-value-at board coord v))]
        solution))))

(defn solve [board]
  (first (rec-solve board))
  )
