(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

;; (def newspaper-sudoku-board
;;   (board [[0 0 6 0 3 0 0 0 0]
;;           [0 0 0 0 0 9 0 0 0]
;;           [4 0 0 0 2 0 0 1 3]
;;           [2 0 1 0 0 5 0 3 0]
;;           [0 3 9 7 0 0 0 2 8]
;;           [0 0 0 3 0 0 0 0 0]
;;           [0 0 0 0 7 0 4 0 0]
;;           [0 0 5 0 0 0 9 0 0]
;;           [0 1 2 6 0 0 0 8 0]]))

;; (def sudoku-board
;;   (board [[5 3 0 0 7 0 0 0 0]
;;           [6 0 0 1 9 5 0 0 0]
;;           [0 9 8 0 0 0 0 6 0]
;;           [8 0 0 0 6 0 0 0 3]
;;           [4 0 0 8 0 3 0 0 1]
;;           [7 0 0 0 2 0 0 0 6]
;;           [0 6 0 0 0 0 2 8 0]
;;           [0 0 0 4 1 9 0 0 5]
;;           [0 0 0 0 8 0 0 7 9]]))



;; (def solved-board
;;   (board [[5 3 4 6 7 8 9 1 2]
;;           [6 7 2 1 9 5 3 4 8]
;;           [1 9 8 3 4 2 5 6 7]
;;           [8 5 9 7 6 1 4 2 3]
;;           [4 2 6 8 5 3 7 9 1]
;;           [7 1 3 9 2 4 8 5 6]
;;           [9 6 1 5 3 7 2 8 4]
;;           [2 8 7 4 1 9 6 3 5]
;;           [3 4 5 2 8 6 1 7 9]]))

(defn value-at [board coord]
  (get-in board coord))

;(value-at newspaper-sudoku-board [8 7])
;(value-at sudoku-board [0 1]) ;=> 3
;(value-at sudoku-board [0 0]) ;=> 5

(defn has-value? [board coord]
  (not= (value-at board coord) 0))

;(has-value? sudoku-board [0 0]) ;=> true
;(has-value? sudoku-board [0 2]) ;=> false

(defn row-values [board coord]
  (let [[x _] coord
        fullrow (get board x)]
  (into #{0} (vec (remove zero? fullrow)))))

;((value-at board [(get board x) (range 0 4)])))))

;(row-values sudoku-board [0 2]) ;=> #{0 5 3 7}
;(row-values sudoku-board [3 2]) ;=> #{0 8 6 3}
;(row-values sudoku-board [5 2])

(defn col-values [board coord]
  (let [[_ y] coord
    transposed-board (vec (apply map list board))
        fullrow (get transposed-board y)]
    (into #{0} (vec (remove zero? fullrow)))))

;(col-values sudoku-board [0 2]) ;=> #{0 8}
;(col-values sudoku-board [4 8]) ;=> #{3 1 6 0 5 9}

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    (vec [x y])))

;; (coord-pairs [0 1])   ;=> [[0 0] [0 1]
;;                       ;    [1 0] [1 1]]

;; (coord-pairs [0 1 2]) ;=> [[0 0] [0 1] [0 2]
;;                       ;    [1 0] [1 1] [1 2]
;;                       ;    [2 0] [2 1] [2 2]]

(defn block-values [board coord]
  nil)

;(block-values sudoku-board [0 2]) ;=> #{0 5 3 6 8 9}
;(block-values sudoku-board [4 5]) ;=> #{0 6 8 3 2}

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
