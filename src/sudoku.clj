(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(def sudoku-board2
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

(def solved-board2
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
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (reduce #(conj %1 (get %2 col))
            #{}
            board)))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    (vector x y)))

(defn block-coords [coords]
  (let [[row col] coords
        block-coord #(* (int (/ % 3)) 3)]
    [(block-coord row)
     (block-coord col)]))

(defn block-values [board coord]
  (let [[b-row b-col] (block-coords coord)]
    (set (map #(let [[row col] %]
                 (value-at board [(+ b-row row) (+ b-col col)]))
              (coord-pairs [0 1 2])))))


(defn valid-values-for [board coord]
  (set/difference
     all-values
     (block-values board coord)
     (row-values board coord)
     (col-values board coord)))

(defn all-board-values [board]
  (reduce #(apply conj %1 %2) #{} board))

(defn filled? [board]
  (not (contains? (all-board-values board) 0)))

(defn rows [board]
  (map set board))

(defn valid-sets? [sets]
  (reduce #(and %1 %2) (map #(= % all-values) sets)))

(defn valid-rows? [board]
  (valid-sets? (rows board)))

(defn cols [board]
  (map #(col-values board [0 %])
       (take 9 (range))))

(defn valid-cols? [board]
  (valid-sets? (cols board)))

(defn blocks [board]
  (map (partial block-values board) (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
    (valid-sets? (blocks board)))

(defn valid-solution? [board]
  (and (valid-blocks? board)
       (valid-cols? board)
       (valid-rows? board)
       true))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (drop-while #(if (nil? %)
                        false
                        (> (value-at board %) 0))
                     (coord-pairs (take 9 (range))))))

(defn solve [board]
  (if (valid-solution? board)
    board
    (let [a-coord (find-empty-point board)]
      (if (nil? a-coord)
        '()
        (for [value (valid-values-for board a-coord)
              solution (solve (set-value-at board a-coord value))]
          solution)))))
