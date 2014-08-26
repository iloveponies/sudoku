(use 'sudoku :reload)

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

(def invalid-board
  (board [[5 3 4 6 7 8 9 1 2]
          [6 7 2 1 9 5 3 4 8]
          [1 9 8 3 4 7 5 6 7]
          [8 5 9 7 6 1 4 2 3]
          [4 2 6 8 5 3 7 9 1]
          [7 1 3 9 2 4 8 5 6]
          [9 6 1 5 3 7 2 8 4]
          [2 8 7 4 1 9 6 3 5]
          [3 4 5 2 8 6 1 7 9]]))

(valid-rows? solved-board)  ;=> truthy
(valid-rows? invalid-board) ;=> falsey
(valid-cols? solved-board)  ;=> truthy
(valid-cols? invalid-board) ;=> falsey
(valid-blocks? solved-board)  ;=> truthy
(valid-blocks? invalid-board) ;=>

(valid-solution? solved-board)  ;=> truthy
(valid-solution? invalid-board) ;=> falsey

(set-value-at sudoku-board [1 1] 6)
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

(=
 (set-value-at before-change [2 1] 4)
 after-change)

(class all-coords)
(take 10 all-coords)
(filled? solved-board)

(take 3 (empty-points solved-board))
(find-empty-point sudoku-board)
(value-at sudoku-board [0 2])

;; Write the function (solve board) which takes a sudoku board as a parameter
;; and returns a valid solution to the given sudoku.
;;   (solve sudoku-board) => solved-board
;; Recap of backtracking:
;; check if you are at the end
;; if so, is the solution valid?
;;    if not, return an empty sequence
;;       otherwise return [solution]
;; if not
;;    select an empty location
;;    try solving with each valid value for that location

(defn sum [a-seq]
  (reduce + a-seq))

(defn subset-sum-helper [a-set current-set target]
  (if (= (sum current-set) target)
    [current-set]
    (let [remaining (clojure.set/difference a-set current-set)]
      (for [elem remaining
            solution (subset-sum-helper a-set
                                        (conj current-set elem)
                                        target)]
        solution))))

(defn subset-sum [a-set target]
  (subset-sum-helper a-set #{} target))

(subset-sum #{1 3 4 10 9 23} 50)

(empty-points sudoku-board)

;; (map valid-solution?
;; (for [point (empty-points sudoku-board)
;;       option (valid-values-for sudoku-board point)]

(valid-values-for sudoku-board [0 2])
(set-value-at sudoku-board [0 2] 1)


(solve sudoku-board)





