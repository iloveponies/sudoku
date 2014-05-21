(ns sudoku
  (:require [clojure.set  :as set]
            [clojure.test :as ctest]))

;; board formatting function (does nothing as of now)
(def board identity)

;; board definition for testing
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

;; Exercise 1
;; Write the function (value-at board coordinates) that returns the value at coordinate in board:
;; signature: nested vector, vector -> number
;; purpose: get the number at the given coord
;; stub:
;; (defn value-at [board coord]
;;   -1)
;;
(defn value-at [board coord]
  (get-in board coord))
;;
(ctest/is (= (value-at sudoku-board [0 1])  3))
(ctest/is (= (value-at sudoku-board [0 0])  5))

;; Exercise 2
;; Write the function (has-value? board coordinates) that returns false if the square at coordinates is empty (has 0), and otherwise true.
;; signature: nested vector, vector -> bool
;; purpose: check the given coord for non-zero value
;; stub:
;; (defn has-value? [board coord]
;;   nil)
;;
(defn has-value? [board coord]
  (not (zero? (value-at board coord))))
;;
(ctest/is (= (has-value? sudoku-board [0 0])  true))
(ctest/is (= (has-value? sudoku-board [0 2])  false))

;; Exercise 3
;; Write the function (row-values board coordinates) that returns a set with all numbers on the row of the coordinates
;; Remember that you can use destructing inside the parameter vector to get the row.
;; signature: nested vector, vector -> set
;; purpose: get a set of row values
;; stub:
;; (defn row-values [board coord]
;;   #{})
;;
(defn row-values [board coord]
  (let [[row col] coord]
    (set (get board row))))
;;
(ctest/is (= (row-values sudoku-board [0 2])  #{0 5 3 7}))
(ctest/is (= (row-values sudoku-board [3 2])  #{0 8 6 3}))

;; Exercise 4
;; Write the function (col-values board coordinates) that returns a set with all numbers on the col of the coordinates
;; signature: nested vector, vector -> set
;; purpose: get a set of col values
;; stub:
;; (defn col-values [board coord]
;;   #{})
;;
(defn col-values [board coord]
  (let [[row col] coord]
    ;; map to rows (vectors) of boards
    (set (map #(get % col) board))))
;;
(ctest/is (= (col-values sudoku-board [0 2])  #{0 8}))
(ctest/is (= (col-values sudoku-board [4 8])  #{3 1 6 0 5 9}))


;; Exercise 5
;; Write the function (coord-pairs coord-sequence) that returns all coordinate-pairs of the form [row col] where row is from coord-sequence and col is from coord-sequence.
;; signature: vector -> vector of vectors
;; purpose: Create all possible pairs of given numbers in a vector
;; stub:
;; (defn coord-pairs [coords]
;;   [[] []])
;;
(defn coord-pairs [coords]
  (for [a coords
        b coords]
    [a b]))
;;
(ctest/is (= (coord-pairs [0 1])    [[0 0] [0 1]
                                     [1 0] [1 1]]))
(ctest/is (= (coord-pairs [0 1 2])  [[0 0] [0 1] [0 2]
                                     [1 0] [1 1] [1 2]
                                     [2 0] [2 1] [2 2]]))

;; Exercise 6
;; Write the function (block-values board coordinates) that returns a set with all numbers in the block of coordinates.
;; You might want to write a helper function that returns the coordinates for the top left corner of the block.
;;
;; signature: vector of vectors, vector -> vector
;; purpose: create block coord [0 0]...[2 2]
;; stub:
;; (defn block-coord [board coord]
;;   [nil nil])
;;
(defn block-coord [board coord]
  ;; block-size = sqrt(nrow(board))
  (let [block-size (int (Math/sqrt (count board)))]
    (map #(quot % block-size) coord)))
;;
(ctest/is (= (block-coord sudoku-board [0 2])  [0 0]))
(ctest/is (= (block-coord sudoku-board [4 5])  [1 1]))
;;
;; signature: vector of vectors, vector -> set
;; purpose: get values in the corresponding block
;; stub:
;; (defn block-values [board coord]
;;   #{})
;;
(defn block-values [board coord]
  (->> (let [block-size       (int (Math/sqrt (count board)))
             block-coord      (block-coord board coord)
             left-upper-coord (map #(* % block-size) block-coord)]
         ;;
         ;; all possible combinations
         (for [row (range (first left-upper-coord)  (+ (first left-upper-coord) block-size))
               col (range (second left-upper-coord) (+ (second left-upper-coord) block-size))]
           ;; value at coord
           (value-at board [row col])))
       ;;
       ;; turn sequence into a set
       (set,  )
       ))
;;
(ctest/is (= (block-values sudoku-board [0 2])  #{0 5 3 6 8 9}))
(ctest/is (= (block-values sudoku-board [4 5])  #{0 6 8 3 2}))


;; Exercise 7
;; Write the function (valid-values-for board coordinates) that returns a set with all valid numbers for the square at coordinates.
;; If the square at coordinates already has a value, valid-values should return the empty set #{}.
;; Remember that we already defined the set all-values .
;;
;; sig: vector -> set
;; purpose: return a set of valid values for a cell
;; (defn valid-values-for [board coord]
;;   #{})
;; Create all possible values, 
(defn valid-values-for [board coord]
  (let [all-set (set (range 1 (inc (count board))))]
    ;;
    (->> all-set
         (#(set/difference % (row-values   board coord)),  )
         (#(set/difference % (col-values   board coord)),  )
         (#(set/difference % (block-values board coord)),  )
         )
    ))
;;
(row-values sudoku-board [0 0])
(col-values sudoku-board [0 0])
(block-values sudoku-board [0 0])
(ctest/is (= (valid-values-for sudoku-board [0 0])  #{1 2}))    ; this was wrong on website #{}
(ctest/is (= (valid-values-for sudoku-board [0 2])  #{1 2 4}))


;; Exercise 8
;; Write the function (filled? board) which returns true if there are no empty squares in board, and otherwise false.
;; It might help to write a helper function that returns all numbers of the board in a sequence.
;; Remember that (contains? set element) can be used to check if element is in set.
;;
;; sig: vector of vectors -> bool
;; purpose: check the whole board for absence of zeros
;; stab
;; (defn filled? [board]
;;   nil)
;;
(defn filled? [board]
  (not (contains? (set (flatten board)) 0)))
;;
(ctest/is (= (filled? sudoku-board)  false))
(ctest/is (= (filled? solved-board)  true))


(defn rows [board]
  nil)

(defn valid-rows? [board]
  nil)

(defn cols [board]
  nil)

(defn valid-cols? [board]
  nil)

(defn blocks [board]
  nil)

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
