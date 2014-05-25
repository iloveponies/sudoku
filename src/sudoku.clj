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
(ctest/is (= (value-at sudoku-board [0 1])  3))
(ctest/is (= (value-at sudoku-board [0 0])  5))

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
(ctest/is (= (has-value? sudoku-board [0 0])  true))
(ctest/is (= (has-value? sudoku-board [0 2])  false))

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
(ctest/is (= (row-values sudoku-board [0 2])  #{0 5 3 7}))
(ctest/is (= (row-values sudoku-board [3 2])  #{0 8 6 3}))

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
(ctest/is (= (col-values sudoku-board [0 2])  #{0 8}))
(ctest/is (= (col-values sudoku-board [4 8])  #{3 1 6 0 5 9}))


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
(ctest/is (= (coord-pairs [0 1])    [[0 0] [0 1]
                                     [1 0] [1 1]]))
(ctest/is (= (coord-pairs [0 1 2])  [[0 0] [0 1] [0 2]
                                     [1 0] [1 1] [1 2]
                                     [2 0] [2 1] [2 2]]))

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
(ctest/is (= (filled? sudoku-board)  false))
(ctest/is (= (filled? solved-board)  true))


;;; Exercise 9
;; Write the function (rows board) that returns a sequence of value sets for each row of board. That is, the first set in (rows board) is a set that has every element of the first row of board as element and so on.
;;
;; sig: vector of vectors -> vector of sets
;; purpose: create a set for each row vector
(defn rows [board]
  (map set board))
;; 
(ctest/is (= (rows sudoku-board)  [#{5 3 0 7}
                                   #{6 0 1 9 5}
                                   #{0 9 8 6}
                                   #{8 0 6 3}
                                   #{4 0 8 3 1}
                                   #{7 0 2 6}
                                   #{0 6 2 8}
                                   #{0 4 1 9 5}
                                   #{0 8 7 9}]))

(ctest/is (= (rows solved-board)  [#{1 2 3 4 5 6 7 8 9}
                                   #{1 2 3 4 5 6 7 8 9}
                                   #{1 2 3 4 5 6 7 8 9}
                                   #{1 2 3 4 5 6 7 8 9}
                                   #{1 2 3 4 5 6 7 8 9}
                                   #{1 2 3 4 5 6 7 8 9}
                                   #{1 2 3 4 5 6 7 8 9}
                                   #{1 2 3 4 5 6 7 8 9}
                                   #{1 2 3 4 5 6 7 8 9}]))
;;
;; sig: vector of vectors -> vector of sets
;; purpose: create a set for each column
(defn cols [board]
  ;; Transpose the board
  (->> (apply map vector board)
       ;; Apply rows
       (rows ,  )))
;; Write the function (cols board) that returns the values of each column in board as a sequence of sets.
(ctest/is (= (cols sudoku-board) [#{5 6 0 8 4 7}
                                  #{3 0 9 6}
                                  #{0 8}
                                  #{0 1 8 4}
                                  #{7 9 0 6 2 1 8}
                                  #{0 5 3 9}
                                  #{0 2}
                                  #{0 6 8 7}
                                  #{0 3 1 6 5 9}]))

(ctest/is (= (cols solved-board) [#{1 2 3 4 5 6 7 8 9}
                                  #{1 2 3 4 5 6 7 8 9}
                                  #{1 2 3 4 5 6 7 8 9}
                                  #{1 2 3 4 5 6 7 8 9}
                                  #{1 2 3 4 5 6 7 8 9}
                                  #{1 2 3 4 5 6 7 8 9}
                                  #{1 2 3 4 5 6 7 8 9}
                                  #{1 2 3 4 5 6 7 8 9}
                                  #{1 2 3 4 5 6 7 8 9}]))



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
(ctest/is (= (blocks sudoku-board) [#{5 3 0 6 9 8}
                                    #{0 7 1 9 5}
                                    #{0 6}
                                    #{8 0 4 7}
                                    #{0 6 8 3 2}
                                    #{0 3 1 6}
                                    #{0 6}
                                    #{0 4 1 9 8}
                                    #{2 8 0 5 7 9}]))
(ctest/is (= (blocks solved-board) [#{1 2 3 4 5 6 7 8 9}
                                    #{1 2 3 4 5 6 7 8 9}
                                    #{1 2 3 4 5 6 7 8 9}
                                    #{1 2 3 4 5 6 7 8 9}
                                    #{1 2 3 4 5 6 7 8 9}
                                    #{1 2 3 4 5 6 7 8 9}
                                    #{1 2 3 4 5 6 7 8 9}
                                    #{1 2 3 4 5 6 7 8 9}
                                    #{1 2 3 4 5 6 7 8 9}]))



;;; Exercise 11
;; Write the function (valid-rows? board) that returns true if every row in board is a valid filled row.
;; sig: vector of vectors -> bool
;; purpose: Check if all rows have all numbers
(defn valid-rows? [board]
  (let [board-size (first (distinct (map count board)))]
    ;; check if all sets are of the length of board-size
    (every? #(= (count %) board-size) (rows board))))
;;
(ctest/is (= (valid-rows? solved-board)   true))
(ctest/is (= (valid-rows? sudoku-board)  false))
;;
;; Write the function (valid-cols? board) that returns true if every row in board is a valid filled column.
(defn valid-cols? [board]
  ;; transpose
  (->> (apply map vector board)
       ;; Apply valid-rows?
       (valid-rows? ,  )))
;;
(ctest/is (= (valid-cols? solved-board)   true))
(ctest/is (= (valid-cols? sudoku-board)  false))


;; Write the function (valid-blocks? board) that returns true if every block in board is a valid filled block.
;; sig: vector of vectors -> bool
;; purpose check all blocks
(defn valid-blocks? [board]
  (let [board-size (first (distinct (map count board)))]
    ;; check if all sets are of the length of board-size
    (every? #(= (count %) board-size) (blocks board))))
;;
(ctest/is (= (valid-blocks? solved-board)   true))
(ctest/is (= (valid-blocks? sudoku-board)  false))



;;; Exercise 12
;; Write the function (valid-solution? board) that returns true if board is a valid solution to sudoku.
;; sig: vector of vectors -> bool
(defn valid-solution? [board]
  ;; check by all three valid checkers
  (every? #(% board) [valid-rows? valid-cols? valid-blocks?]))
;;
(ctest/is (= (valid-solution? solved-board)   true))
(ctest/is (= (valid-solution? sudoku-board)  false))




(assoc-in [[:a :b] [:c :d]] [1                                  0] :E)
;=> (assoc [[:a :b] [:c :d]]  1 (assoc (get [[:a :b] [:c :d]] 1) 0  :E))
;=> (assoc [[:a :b] [:c :d]]  1 (assoc               [:c :d]     0  :E))
;=> (assoc [[:a :b] [:c :d]]  1 [:E :d])
;=>        [[:a :b] [:E :d]]



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
(def before-change
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))
(def after-change
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 4 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))
(ctest/is (= (set-value-at before-change [2 1] 4) after-change))


;;; Exercise 14
;; Write the function (find-empty-point board) that returns coordinates to an empty point (that is, in our representation has value 0).
;;
;; sig: vector of vectors -> vector of vectors
;; purpose: given board return vectors of coords
;; (defn find-empty-point [board]
;;   [nil nil])
;;
(defn find-empty-point [board]
  (let [board-size (first (distinct (map count board)))]
    ;; generate all possible coords
    (->> (for [a (range 0 board-size)
               b (range 0 board-size)]
           [a b])
         ;;
         (filter #(zero? (value-at board %)),  )
         )))
;;
(def two-more-to-go
  (board [[5 3 4 6 7 8 9 1 2]
          [6 0 2 1 9 5 3 4 8]
          [1 9 8 3 4 2 5 6 7]
          [8 5 9 7 6 1 4 2 3]
          [4 2 6 8 5 3 7 0 1]
          [7 1 3 9 2 4 8 5 6]
          [9 6 1 5 3 7 2 8 4]
          [2 8 7 4 1 9 6 3 5]
          [3 4 5 2 8 6 1 7 9]]))
;;
(ctest/is (= (find-empty-point two-more-to-go) [[1 1] [4 7]]))
(ctest/is (= (map #(valid-values-for two-more-to-go %) (find-empty-point two-more-to-go)) '(#{7} #{9})))


;;; subset sum as an example of
;; sum functions
(defn sum [a-seq]
  (reduce + a-seq))
;;
(defn subset-sum-helper [a-set current-set target]
  ;; check sum of current set
  (if (= (sum current-set) target)
    ;; if equal, return current set
    [current-set]
    ;; Otherwise,
    (let [remaining (clojure.set/difference a-set current-set)]
      
      (for [;; for each remaining element
            elem remaining
            ;; for each element, add it to the current set, and recur
            solution (subset-sum-helper a-set
                                        (conj current-set elem)
                                        target)]
        ;; return solution for each element remaining
        solution)
      ;; returned as a list of sets
      )))
;;
;; duplication
;; (defn subset-sum [a-set target]
;;   (subset-sum-helper a-set #{} target))
;; just get the first
(defn subset-sum [a-set target]
  (first (subset-sum-helper a-set #{} target)))
;;
(ctest/is (= (subset-sum #{1 2 10 5 7} 23) #{1 5 7 10}))
(ctest/is (= (subset-sum #{1 3 4 10 9 23} 20) #{1 9 10}))



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
(defn filled-valid? [board]
  (and (filled? board)
       (valid-solution? board)))
(ctest/is (= (filled-valid? sudoku-board) false))
;;

(map #(valid-values-for sudoku-board %) (find-empty-point sudoku-board))




















