(ns sudoku
  (:require [clojure.set :as set]))


(def board identity)


(def worlds-hardest-board ;; according to: http://www.telegraph.co.uk/news/science/science-news/9359579/Worlds-hardest-sudoku-can-you-crack-it.html
  (board [[8 0 0 0 0 0 0 0 0]
          [0 0 3 6 0 0 0 0 0]
          [0 7 0 0 9 0 2 0 0]
          [0 5 0 0 0 7 0 0 0]
          [0 0 0 0 4 5 7 0 0]
          [0 0 0 1 0 0 0 3 0]
          [0 0 1 0 0 0 0 6 8]
          [0 0 8 5 0 0 0 1 0]
          [0 9 0 0 0 0 4 0 0]]))


;; Ex 1
;; Write the function (value-at board coordinates) that returns the value at coordinate in board:

(defn value-at [board coord]
  (let [[row col] coord]
    (get-in board [row col])))


;; Ex 2
;; Write the function (has-value? board coordinates) that returns false if the square at coordinates is empty (has 0), and otherwise true.

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))


;; Ex 3
;; Write the function (row-values board coordinates) that returns a set with all numbers on the row of the coordinates
;; Remember that you can use destructing inside the parameter vector to get the row.

(defn row-values [board coord]
  (let [[row _] coord]
   (into #{} (nth board row))))


;; Ex 4
;; Write the function (col-values board coordinates) that returns a set with all numbers on the col of the coordinates

(defn col-values [board coord]
  (let [[_ col] coord]
    (into #{} (map #(nth % col) board))))


;; Ex 5
;; Write the function (coord-pairs coord-sequence) that returns all coordinate-pairs of the form [row col] where row is from coord-sequence and col is from coord-sequence.

(defn coord-pairs [coord-sequence]
  (for [x coord-sequence y coord-sequence] [x y]))


;; Ex 6
;; Write the function (block-values board coordinates) that returns a set with all numbers in the block of coordinates.
;; You might want to write a helper function that returns the coordinates for the top left corner of the block.

; Helpers:
; Use defs here as they are evaluated only once.

(def all-values #{1 2 3 4 5 6 7 8 9})

(def coord->block
  (into {}
    (for [y (range 9) x (range 9)]
      (vector [x y] (+ (* (quot x 3) 3) (quot y 3))))))

(def block->coordlist
  (into {}
    (reduce (fn [dict [coord b]] (assoc dict b (cons coord (get dict b)))) {} coord->block)))

(defn coord->coordlist [coord]
  (-> coord coord->block block->coordlist))

(defn block-values [board coord]
  (set (map #(value-at board %) (coord->coordlist coord))))


;; Ex 7
;; Write the function (valid-values-for board coordinates) that returns a set with all valid numbers for the square at coordinates.
;; If the square at coordinates already has a value, valid-values should return the empty set #{}.
;; Remember that we already defined the set all-values.

(defn valid-values-for [board coord]
  (if (not (zero? (value-at board coord)))
    #{}
    (set/difference all-values (set/union (block-values board coord) (row-values board coord) (col-values board coord)))))


;; Ex 8
;; Write the function (filled? board) which returns true if there are no empty squares in board, and otherwise false.
;; It might help to write a helper function that returns all numbers of the board in a sequence.
;; Remember that (contains? set element) can be used to check if element is in set.

(defn filled? [board]
  (not (contains? (reduce (fn [s row] (set/union s (set row))) #{} board) 0)))


;; Ex 9
;; Write the function (rows board) that returns a sequence of value sets for each row of board. That is, the first set in (rows board) is a set that has every element of the first row of board as element and so on.

(defn rows [board]
  (apply vector (map #(into #{} %) board)))


(defn cols [board]
  (apply vector (for [col (range 9)]
    (set (map #(nth % col) board)))))


;; Ex 10
;; Write the function (blocks board) that returns the values of each block in board as a sequence of sets.

(defn blocks [board]
  (apply vector (for [coord block->coordlist]
    (set (map (partial value-at board) (nth coord 1))))))


;; Ex 11
;; Write the function (valid-rows? board) that returns true if every row in board is a valid filled row.

(defn valid-rows? [board]
  (every? #(= all-values (set %)) (rows board)))

;; Ex 11 - contd.
;; Write the function (valid-cols? board) that returns true if every row in board is a valid filled column.

(defn valid-cols? [board]
  (every? #(= all-values (set %)) (cols board)))

;; Ex 11 - contd.
;; Write the function (valid-blocks? board) that returns true if every block in board is a valid filled block.

(defn valid-blocks? [board]
  (every? #(= all-values (set %)) (blocks board)))


;; Ex 12
;; Write the function (valid-solution? board) that returns true if board is a valid solution to sudoku.

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))


;; Ex 13
;; Write the function (set-value-at board coord new-value) that changes the value at coord in board to new-value

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))


;; Ex 14
;; Write the function (find-empty-point board) that returns coordinates to an empty point (that is, in our representation has value 00).

(defn find-empty-point [board]
  (reduce
    (fn [result coord]
      (if (zero? (get-in board coord))
        (reduced coord)))
    nil
    (for [row (range 9) col (range 9)] [row col])))


;; Ex 15
;; Write the function (solve board) which takes a sudoku board as a parameter and returns a valid solution to the given sudoku.
;;
;; Recap of backtracking:
;;
;;   . check if you are at the end
;;   . if so, is the solution valid?
;;     . if not, return an empty sequence
;;     . otherwise return [solution]
;;   . if not
;;     . select an empty location
;;     . try solving with each valid value for that location

(defn get-move-list
  "Helper to generate a list of new board positions"
  [board]
  (let [next-coord (find-empty-point board)]
    (if next-coord
      (map #(set-value-at board next-coord %) (valid-values-for board next-coord))
      (list))))


(defn solve [board]
  (loop [board       board
         move-list   (list)]
    (if (valid-solution? board)
      board
      (let [new-move-list (lazy-cat (get-move-list board) move-list)]   ;; lazy-cat prevents stack overflow
        (if (empty? new-move-list)
          :unsolvable
          (recur (first new-move-list) (rest new-move-list))
        )))))

;(solve worlds-hardest-board)
