(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(def my-board
  (board [[5 3 0   0 7 0   0 0 0]
          [6 0 0   1 9 5   0 0 0]
          [0 9 8   0 0 0   0 6 0]

          [8 0 0   0 6 0   0 0 3]
          [4 0 0   8 0 3   0 0 1]
          [7 0 0   0 2 0   0 0 6]

          [0 6 0   0 0 0   2 8 0]
          [0 0 0   4 1 9   0 0 5]
          [0 0 0   0 8 0   0 7 9]]))

(def my-solved-board
  (board [[5 3 4   6 7 8   9 1 2]
          [6 7 2   1 9 5   3 4 8]
          [1 9 8   3 4 2   5 6 7]

          [8 5 9   7 6 1   4 2 3]
          [4 2 6   8 5 3   7 9 1]
          [7 1 3   9 2 4   8 5 6]

          [9 6 1   5 3 7   2 8 4]
          [2 8 7   4 1 9   6 3 5]
          [3 4 5   2 8 6   1 7 9]]))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (== (value-at board coord) 0)))

(defn row-values [board [row col]]
  (set (get board row)))

(defn col-values [board [row col]]
  (set (map #(get % col) board)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board coord]
  (let [upper-left-coords (fn [coord] (map #(* 3 (quot % 3)) coord))
        [row col] (upper-left-coords coord)]
    (set
     (for [r (range row (+ row 3))
           c (range col (+ col 3))]
       (value-at board [r c])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                    (row-values board coord)
                    (col-values board coord)
                    (block-values board coord))))

(defn filled? [board]
  (not (contains? (set (apply concat board)) 0)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? #(= % all-values) (rows board)))

(defn cols [board]
 (map set (apply map vector board)))

(defn valid-cols? [board]
  (every? #(= % all-values) (cols board)))

(defn blocks [board]
  (map set
       (for [br (range 0 3)
             bc (range 0 3)]
         (map (partial value-at board)
              (for [r (range 0 3)
                    c (range 0 3)]
                [(+ (* 3 br) r) (+ (* 3 bc) c)])))))

(defn valid-blocks? [board]
  (every? #(= % all-values) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter #(not (has-value? board %)) (coord-pairs (range 0 9)))))

(defn lh [msg val] (printf "%s: %s\n" msg val) val)

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board) board [])
    (let [pos (find-empty-point board)]
      (apply vector
             (for [value    (valid-values-for board pos)
                   solution (solve (set-value-at board pos value))]
               solution)))))
