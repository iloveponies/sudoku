;; Anything you type in here will be executed
;; immediately with the results shown on the
;; right.

(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  "Returns the value at coordinate in board"
  (get-in board coord))

(defn has-value? [board coord]
  "Returns false if the square at coordinates is empty (has 0), and otherwise true."
  (let [myV (value-at board coord)]
    (if (= myV 0)
      false
      true)))

(defn row-values [board coord]
  "Returns a set with all numbers on the row of the coordinates"
  (let [[x _] coord]
    (set (map (fn [n] (value-at board [x n])) (range 0 9)))))

(defn col-values [board coord]
  "Returns a set with all numbers on the col of the coordinates"
  (let [[_ y] coord]
    (set (map (fn [n] (value-at board [n y])) (range 0 9)))))

(defn coord-pairs [coords]
  "Returns all coordinate-pairs of the form [row col]
  where row is from coord-sequence and col is from coord-sequence."
  (vec (for [row coords
        col coords]
    (vec (concat [row col])))))

(defn block-values [board coord]
  "Returns a set with all numbers in the block of coordinates."
  (let [[x y] coord
        xS (- x (mod x 3))
        yS (- y (mod y 3))]
    (set
      (map (fn [x] (value-at board (conj [] (first x) (second x))))
        (map (fn [x] (conj [] (+ (first x) xS ) (+ (second x) yS)))
          (coord-pairs [0 1 2]))))))

(defn valid-values-for [board coord]
  "Returns a set with all valid numbers for the square at coordinates.
  If the square at coordinates already has a value,
  valid-values should return the empty set #{}."
  (if (= 0 (value-at board coord))
    (set/difference all-values
                    (block-values board coord)
                    (row-values board coord)
                    (col-values board coord))
    #{}))

(defn filled? [board]
  "Returns true if there are no empty squares in board, and otherwise false."
  (if (contains? (set (reduce concat board)) 0)
    false
    true))

(defn rows [board]
  "Returns a sequence of value sets for each row of board.
  That is, the first set in (rows board) is a set that has every element
  of the first row of board as element and so on."
  (let [enum (range 0 9)]
    (for [x enum] (set (for [y enum] (value-at board [x y]))))))

(defn valid-rows? [board]
  "Returns true if every row in board is a valid filled row."
  (let [theBoardRows (rows board)
        numbersInRows (reduce set/union theBoardRows)]
    (if (and ((complement contains?) numbersInRows 0) (= (apply min (map count theBoardRows)) 9))
      true
      false)))

(defn cols [board]
  "Returns the values of each column in board as a sequence of sets."
  (let [enum (range 0 9)]
    (map set
         (map (fn [y] (map (fn [x] (value-at board [x y]))
                           enum))
              enum))))

(defn valid-cols? [board]
  "Returns true if every row in board is a valid filled column."
  (let [theBoardCols (cols board)
        numbersInCols (reduce set/union theBoardCols)]
    (if (and ((complement contains?) numbersInCols 0)
             (= (apply min (map count theBoardCols)) 9))
      true
      false)))

(defn blocks [board]
  "Returns the values of each block in board as a sequence of sets."
  (let [enum (list 0 3 6)]
    (vec
      (reduce concat
        (map (fn [x]
               (map (fn [y] (block-values board [x y]))
                    enum)) enum)))))

(defn valid-blocks? [board]
  "Returns true if every block in board is a valid filled block."
  (let [theBoardBlocks (blocks board)
        numbersInBlocks (reduce set/union theBoardBlocks)]
    (if (and ((complement contains?) numbersInBlocks 0)
             (= (apply min (map count theBoardBlocks)) 9))
      true
      false)))

(defn valid-solution? [board]
  "Returns true if board is a valid solution to sudoku."
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  "Changes the value at coord in board to new-value."
  (assoc-in board coord new-value))

(defn find-empty-point-helper [board coords]
  (if (empty? coords)
    nil
    (if (not (has-value? board (first coords)))
      (first coords)
      (find-empty-point-helper board (rest coords)))))

(defn find-empty-point [board]
  (let [coords
        (apply concat
          (for [y (range 0 9)]
            (for [x (range 0 9)]
              [y x])))]
    (find-empty-point-helper board coords)))

(defn solve-helper [current]
  (if (filled? current)
    (if (valid-solution? current)
      [current]
      [])
    (let [empty-coord (find-empty-point current)]
      (for [value (valid-values-for current empty-coord)
            solution (solve-helper
                       (set-value-at current empty-coord value))]
        solution))))

(defn solve [board]
  "Takes a sudoku board as a parameter and returns a valid solution to the given sudoku."
  (first (solve-helper board)))









