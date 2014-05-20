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


(defn coord-pairs [coords]
  nil)

(defn block-values [board coord]
  nil)

(defn valid-values-for [board coord]
  nil)

(defn filled? [board]
  nil)

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
