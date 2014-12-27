(ns sudoku
  (:require [clojure.set :as set]))

(defn reload []
  (use 'sudoku :reload))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(def sudoku-boardx
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

(def solved-boardx
  (board [[5 3 4 6 7 8 9 1 2]
          [6 7 2 1 9 5 3 4 8]
          [1 9 8 3 4 2 5 6 7]
          [8 5 9 7 6 1 4 2 3]
          [4 2 6 8 5 3 7 9 1]
          [7 1 3 9 2 4 8 5 6]
          [9 6 1 5 3 7 2 8 4]
          [2 8 7 4 1 9 6 3 5]
          [3 4 5 2 8 6 1 7 9]]))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (set (map #(value-at board [%1 (last coord)]) (range 9))))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn get-upper-left [coord]
  (map #(* 3 (quot % 3)) coord))

(defn get-coords-for-block-upper-left [coord]
  "argument is upper left, returns block coords for those"
  (map #(list (+ (first coord) (first %)) (+ (last coord) (last %))) (coord-pairs [0 1 2])))

(defn block-values [board coord]
  (set
   (for [c (get-coords-for-block-upper-left (get-upper-left coord))]
     (value-at board c)
    )))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (block-values board coord) (col-values board coord) (row-values board coord))))

(defn check-no-zero-in-set? [set]
    (not (contains? set 0)))

(defn filled? [board]
  (check-no-zero-in-set? (reduce #(into %1 %2) #{} board)))

(defn rows [board]
  (map #(set %) board))

(defn every-set-equal-to-all-values? [sets]
  (every? #(= % all-values) sets))

(defn valid-rows? [board]
  (every-set-equal-to-all-values? (rows board)))

(defn cols [board]
  (for [x (range 9)]
    (col-values board [0 x])
    ))

(defn valid-cols? [board]
  (every-set-equal-to-all-values? (cols board)))

(defn blocks [board]
  (for [x (range 3)
        y (range 3)]
    (block-values board [(* 3 x) (* 3 y)])
    ))

(defn valid-blocks? [board]
  (every-set-equal-to-all-values? (blocks board)))

(defn valid-solution? [board]
  (and (valid-blocks? board) (valid-cols? board) (valid-rows? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-all-empty-points [board]
  (for [x (range 9)]
    (let [row (get board x)]
      (when (some #(= 0 %) row)
        [x (.indexOf row 0)]
      ))))

(defn find-empty-point [board]
  (first (filter identity (find-all-empty-points board))))


(defn solve-helper [board empty-coord]
  "- check if filled and after that iff check if valid. iff, return solution.
   - get one valid value, set it to coords and then continue to the next empty with recursion.
   "
  (if (and (filled? board) (valid-solution? board))
    [board]
    (let [valid-values (valid-values-for board empty-coord)
          new-board (set-value-at board empty-coord 1)
          coord (find-empty-point new-board)]
      (for [elem valid-values
            solution (solve-helper (set-value-at board empty-coord elem) coord)]
        solution
        )
      )
    )
  )

(defn solve [board]
  (let [coord  (find-empty-point board)]
    (first (solve-helper board coord))))
