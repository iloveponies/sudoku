(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  "Returns value at coordinate."
  (get-in board coord))

(defn has-value? [board coord]
  "Returns true if coordinate is not null."
  (not (= 0
        (value-at board coord))))

(defn row-values [board coord]
  "Returns a set of row's values."
  (let [[row _] coord]
    (set (get board row))))

(defn col-values [board coord]
  "Returns a set of column's values."
  (let [[_ col] coord]
    (set
     (for [i (range 0 9)]
       (value-at board [i col])))))

(defn coord-pairs [coords]
  "Returns coordinate pairs."
  (for [i coords
        j coords]
    [i j]))

(defn block-coords [[x y]]
  "Returns the coordinates for each position in the same block as parameter."
  (let [xrange (range (* 3 (int (/ x 3)))
                      (+ 3 (* 3 (int (/ x 3)))))
        yrange (range (* 3 (int (/ y 3)))
                      (+ 3 (* 3 (int (/ y 3)))))]

    (for [i xrange
          j yrange]
      [i j])))

(defn block-values [board coord]
  "Returns block's values as a set."
  (let [f (fn [x] (value-at board x))]
    (set (map f (block-coords coord)))))

(def full-set (set (range 1 10)))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference full-set
                    (set/union (row-values board coord)
                               (col-values board coord)
                               (block-values board coord)))))

(defn filled? [board]
  "Checks if every coordinate has a value."
  (every?
   (fn [x] (has-value? board x))
   (coord-pairs (range 0 9))))

(defn rows [board]
  "Lists all rows as sets."
  (for [i (range 0 9)]
    (row-values board [i 0])))

(defn valid-rows? [board]
  "Checks if all rows are valid."
  (every? (fn [a-set] (= full-set a-set))
          (rows board)))

(defn cols [board]
  "Lists all columns as sets."
  (for [i (range 0 9)]
    (col-values board [0 i])))

(defn valid-cols? [board]
  "Checks if all columns are valid."
  (every? (fn [a-set] (= full-set a-set))
          (cols board)))

;(coord-pairs [0 3 6])

(defn blocks [board]
  "Lists all blocks as sets."
  (for [coord (coord-pairs [0 3 6])]
    (block-values board coord)))

(defn valid-blocks? [board]
  "Checks if all the blocks are valid."
  (every? (fn [a-set] (= full-set a-set))
          (blocks board)))

(defn valid-solution? [board]
  "Checks if a solution is valid."
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  "Sets a value on the board and returns the new board."
  (assoc-in board coord new-value))


(defn find-all-empty-points [board]
  "Finds all empty points on the board and returns them as a sequence."
  (filter (fn [coord] (= 0 (value-at board coord)))
          (coord-pairs (range 0 9))))

;(defn find-empty-point [board]
;  "Finds one empty point on the board."
;  (first (filter
;          (fn [coord] (= 0 (value-at board coord)))
;          (coord-pairs (range 0 9)))))

(defn only-one-possible? [board coord]
  "Checks if only one possible number exists for a given coordinate."
  (= 1 (count (valid-values-for board coord))))

(defn easy-points [board]
  "Finds all easy points on the board as coordinates (where only 1 possibility exists)."
  (filter (fn [x] (only-one-possible? board x))
          (find-all-empty-points board)))

(defn solve-1 [board]
  "Puts one number in place."
  (let [coord (first (easy-points board))]
    (if (empty? coord)
        board
        (set-value-at board
                      coord
                      (first (valid-values-for board coord))))))

(defn solve [board]
  "Tries to solve the board."
  (loop [b board
         b1 (solve-1 board)]
    (if (= b b1)
        b
        (recur b1
               (solve-1 b1)))))
