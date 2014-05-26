(ns sudoku
  (:require [clojure.set  :as set]
            [clojure.test :as ctest]))


;;; board formatting function (does nothing as of now)
(def board identity)

;;; board definition for testing
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

;;; board-printing function
(defn print-board [board]
  (println (clojure.string/replace (apply str (interpose "\n" board)) #"[0]" " ")))


;;; Exercise 1
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


;;; Exercise 2
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


;;; Exercise 3
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


;;; Exercise 4
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


;;; Exercise 5
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


;;; Exercise 6
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
         (for [row (range (first  left-upper-coord) (+ (first  left-upper-coord) block-size))
               col (range (second left-upper-coord) (+ (second left-upper-coord) block-size))]
           ;; value at coord
           (value-at board [row col])))
       ;;
       ;; turn sequence into a set
       (set,  )
       ))
;;


;;; Exercise 7
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
  ;; Check if the coord is already filled
  (if (not (zero? (value-at board coord)))
    ;; if filled
    #{}
    ;; if not filled (zero)
    (let [all-values  (set (range 1 (inc (count board))))
          used-values (clojure.set/union (row-values   board coord)
                                         (col-values   board coord)
                                         (block-values board coord))]
      ;; set difference
      (clojure.set/difference all-values used-values))))
;;


;;; Exercise 8
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


;;; Exercise 9
;; Write the function (rows board) that returns a sequence of value sets for each row of board. That is, the first set in (rows board) is a set that has every element of the first row of board as element and so on.
;;
;; sig: vector of vectors -> vector of sets
;; purpose: create a set for each row vector
(defn rows [board]
  (map set board))
;;
;;
;; sig: vector of vectors -> vector of sets
;; purpose: create a set for each column
(defn cols [board]
  ;; Transpose the board
  (->> (apply map vector board)
       ;; Apply rows
       (rows ,  )))
;; Write the function (cols board) that returns the values of each column in board as a sequence of sets.


;;; Exercise 10
;; Write the function (blocks board) that returns the values of each block in board as a sequence of sets.
;;
;; sig vector of vectors -> vector of maps
;; purpose: get all values at blocks [0 0] [0 1] [0 2], [1 0]
;; (defn blocks [board]
;;   [#{}])
;;
(defn blocks [board]
  (let [board-size    (first (distinct (map count board)))
        block-size    (int (Math/sqrt board-size))
        ;; return 0, 3, 6 left/upper coords of each block
        corner-coords (range 0 board-size block-size)]
    ;; Cycle through these. b (col) changes faster
    (for [a corner-coords
          b corner-coords]
      ;; get values for the corresponding block
      (block-values board [a b]))))

;;


;;; Exercise 11
;; Write the function (valid-rows? board) that returns true if every row in board is a valid filled row.
;; sig: vector of vectors -> bool
;; purpose: Check if all rows have all numbers
(defn valid-rows? [board]
  (let [board-size (first (distinct (map count board)))]
    ;; check if all sets are of the length of board-size
    (every? #(= (count %) board-size) (rows board))))
;;
;;
;; Write the function (valid-cols? board) that returns true if every row in board is a valid filled column.
(defn valid-cols? [board]
  ;; transpose
  (->> (apply map vector board)
       ;; Apply valid-rows?
       (valid-rows? ,  )))
;;
;; Write the function (valid-blocks? board) that returns true if every block in board is a valid filled block.
;; sig: vector of vectors -> bool
;; purpose check all blocks
(defn valid-blocks? [board]
  (let [board-size (first (distinct (map count board)))]
    ;; check if all sets are of the length of board-size
    (every? #(= (count %) board-size) (blocks board))))
;;


;;; Exercise 12
;; Write the function (valid-solution? board) that returns true if board is a valid solution to sudoku.
;; sig: vector of vectors -> bool
(defn valid-solution? [board]
  ;; check by all three valid checkers
  (every? #(% board) [valid-rows? valid-cols? valid-blocks?]))
;;


;;; Exercise 13
;; Write the function (set-value-at board coord new-value) that changes the value at coord in board to new-value.
;;
;; Signature: vector of vectors, vector, a number -> vector of vectors
;;
;; (defn set-value-at [board coord new-value]
;;   [[] []])
;;
(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))
;;


;;; Exercise 14
;; Write the function (find-empty-point board) that returns coordinates to an empty point (that is, in our representation has value 0).
;;
;; sig: vector of vectors -> vector of vectors
;; purpose: given board return vectors of coords
;; (defn find-empty-point [board]
;;   [nil nil])
;;
;; Function to return all empty coords in a list in the board
(defn empty-coords [board]
  (let [board-size (first (distinct (map count board)))]
    ;; generate all possible coords
    (->> (for [a (range 0 board-size)
               b (range 0 board-size)]
           [a b])
         ;;
         (filter #(zero? (value-at board %)),  ))))
;;
;; This just pick the first one (upper row, closer to left)
(defn find-empty-point [board]
  (first (empty-coords board)))
;;


;;; Exercise 15
;; Write the function (solve board) which takes a sudoku board as a parameter and returns a valid solution to the given sudoku.
;;   (solve sudoku-board) => solved-board
;; Recap of backtracking:
;; check if you are at the end
;; if so, is the solution valid?
;; if not, return an empty sequence
;; otherwise return [solution]
;; if not
;; select an empty location
;; try solving with each valid value for that location
;;
;; sig: vector of vectors -> same
;; purpose enter valid numbers.
;; (defn solve [board]
;;   nil)

;; solve-recur returns the correct solutions many times
;; Fill one unfilled coord with one valid solution,
;; with that recur untill validly filled up.
(defn solve-recur [current-board]
  ;; Get the coord for the first unfilled point
  (let [first-unfilled-coord (find-empty-point current-board)]
    ;; Check if filled (nil? nil) if filled
    (if (nil? first-unfilled-coord)
      ;; if filled, check if valid
      (if (valid-solution? current-board)
        
        ;; if filled and valid, return in a vector
        [current-board]
        ;; if filled and invalid, return an empty sequence
        [])

      ;; If unfilled, fill one location with one valid value (do "for" all possible combinations)
      (for [;; pick one valid value for the first unfilled coord
            valid-value (valid-values-for current-board first-unfilled-coord)
            ;; recurse with that one-more-coord-filled board
            ;; pick one board out of a sequence of validly filled boards
            solution    (solve-recur (set-value-at current-board first-unfilled-coord valid-value))]
        ;; 
        solution))))

;; solve function that pick the first copy of the solutions
;; Actually this is not necessary??
(defn solve [board]
  (first (solve-recur board)))
