(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord)
  )

(defn has-value? [board coord]
 (> (value-at board coord) 0)
  )

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row)))
    )

(defn col-values [board coord]
  (let [[_ col] coord]
    (set (map (fn [x] (get x col)) board)))
  )

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col])
  )

(defn helper-get-top-left [coord]
  (vector (* 3 (int (Math/floor (/ (first coord) 3)))) ; get x
          (* 3 (int (Math/floor (/ (second coord) 3))))) ; get y
  )

(defn block-values [board coord]
  (let [leftie (helper-get-top-left coord)]
    (set (for [rowi (range 3) colj (range 3)]
           (let [coordX (+ rowi (first leftie))
                 coordY (+ colj (second leftie))]
             (value-at board [coordX coordY])))))
  )

(def all-values
#{1 2 3 4 5 6 7 8 9}
  )

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
    (set/difference all-values
                    (block-values board coord)
                    (row-values board coord)
                    (col-values board coord)))
  )

(defn filled? [board]
  (not (some zero? (flatten board))) ; sorry for flatten, this seemed easiest
  )

(defn rows [board]
  (for [x (range 9)]
    (row-values board [x 0]))
  )

(defn valid-rows? [board]
  (every? (fn [row] (= all-values row)) (rows board))
  )

(defn cols [board]
 (for [y (range 9)]
   (col-values board [0 y]))
  )

(defn valid-cols? [board]
  (every? (fn [col] (= all-values col)) (cols board))
  )

(defn blocks [board]
  (for [x (range 3)
        y (range 3)]
    (block-values board [(* 3 x) (* 3 y)]))
  )

(defn valid-blocks? [board]
  (every? (fn [bloc] (= all-values bloc)) (blocks  board))
  )

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board))
  )

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value)
  )

(defn find-empty-point [board]
  (first (filter (fn [coord] (= 0 (value-at board coord))) (coord-pairs (range 0 9))))
  )

(defn solve [board]
  (if (valid-solution? board) board
  (let [emptyCoord (find-empty-point board)]
    (for [new-value (valid-values-for board emptyCoord)
      solution (solve (set-value-at board emptyCoord new-value))] solution)))
  )
