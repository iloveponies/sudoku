(use 'sudoku)

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
(def almost2-solved-board
  (board [[5 3 4 6 7 8 9 1 2]
          [6 7 2 1 9 5 3 4 8]
          [1 0 8 3 4 2 5 6 7]
          [8 5 9 7 6 1 4 2 3]
          [4 2 6 8 5 3 7 9 1]
          [7 1 3 9 2 0 8 5 6]
          [9 6 1 5 3 7 2 8 4]
          [2 8 7 4 1 9 6 3 5]
          [3 4 5 2 8 6 1 7 9]]))
(def almost6-solved-board
  (board [[5 3 4 6 7 8 9 1 2]
          [6 7 2 1 9 5 3 4 8]
          [0 0 8 3 4 0 5 6 7]
          [8 5 9 7 6 1 4 2 3]
          [4 2 6 8 5 0 7 9 1]
          [7 0 3 9 2 0 8 5 6]
          [9 6 1 5 3 7 2 8 4]
          [2 8 7 4 1 9 6 3 5]
          [3 4 5 2 8 6 1 7 9]]))





(value-at sudoku-board [0 1]) ;=> 3
(value-at sudoku-board [0 0]) ;=> 5

(has-value? sudoku-board [0 0]) ;=> true
(has-value? sudoku-board [0 2]) ;=> false

(row-values sudoku-board [0 2]) ;=> #{0 5 3 7}
(row-values sudoku-board [3 2]) ;=> #{0 8 6 3}

(col-values sudoku-board [0 2]) ;=> #{0 8}
(col-values sudoku-board [4 8]) ;=> #{3 1 6 0 5 9}

(coord-pairs [0 1])   ;=> [[0 0] [0 1]
                      ;    [1 0] [1 1]]
(coord-pairs [0 1 2]) ;=> [[0 0] [0 1] [0 2]
                      ;    [1 0] [1 1] [1 2]
                      ;    [2 0] [2 1] [2 2]]
(coord-pairs [1 2 4])

(top-left-block-coord 1 1)
(top-left-block-coord 5 5)
(block-coords [1 1])
(block-coords [5 5])
(block-values sudoku-board [0 2]) ;=> #{0 5 3 6 8 9}
(block-values sudoku-board [4 5]) ;=> #{0 6 8 3 2}

(valid-values-for sudoku-board [0 0]) ;=> #{}
(valid-values-for sudoku-board [0 2]) ;=> #{1 2 4})

(filled? sudoku-board) ;=> false
(filled? solved-board) ;=> true

(rows sudoku-board) ;=> [#{5 3 0 7}
                    ;    #{6 0 1 9 5}
                    ;    #{0 9 8 6}
                    ;    #{8 0 6 3}
                    ;    #{4 0 8 3 1}
                    ;    #{7 0 2 6}
                    ;    #{0 6 2 8}
                    ;    #{0 4 1 9 5}
                    ;    #{0 8 7 9}]

(rows solved-board) ;=> [#{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}]

(cols sudoku-board) ;=> [#{5 6 0 8 4 7}
                    ;    #{3 0 9 6}
                    ;    #{0 8}
                    ;    #{0 1 8 4}
                    ;    #{7 9 0 6 2 1 8}
                    ;    #{0 5 3 9}
                    ;    #{0 2}
                    ;    #{0 6 8 7}
                    ;    #{0 3 1 6 5 9}]

(cols solved-board) ;=> [#{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}]


(blocks sudoku-board) ;=> [#{5 3 0 6 9 8}
                      ;    #{0 7 1 9 5}
                      ;    #{0 6}
                      ;    #{8 0 4 7}
                      ;    #{0 6 8 3 2}
                      ;    #{0 3 1 6}
                      ;    #{0 6}
                      ;    #{0 4 1 9 8}
                      ;    #{2 8 0 5 7 9}]

(blocks solved-board) ;=> [#{1 2 3 4 5 6 7 8 9}
                      ;    #{1 2 3 4 5 6 7 8 9}
                      ;    #{1 2 3 4 5 6 7 8 9}
                      ;    #{1 2 3 4 5 6 7 8 9}
                      ;    #{1 2 3 4 5 6 7 8 9}
                      ;    #{1 2 3 4 5 6 7 8 9}
                      ;    #{1 2 3 4 5 6 7 8 9}
                      ;    #{1 2 3 4 5 6 7 8 9}
                      ;    #{1 2 3 4 5 6 7 8 9}])

(valid-rows? solved-board)  ;=> truthy
(valid-rows? sudoku-board) ;=> falsey
(valid-cols? solved-board)  ;=> truthy
(valid-cols? sudoku-board) ;=> falsey
(valid-blocks? solved-board)  ;=> truthy
(valid-blocks? sudoku-board) ;=> falsey

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

(= (set-value-at before-change [2 1] 4) after-change)

(find-empty-point sudoku-board)
(find-empty-point solved-board)

(= solved-board (solve solved-board))
(= solved-board (solve almost2-solved-board))
(= solved-board almost2-solved-board)
(= solved-board (solve almost6-solved-board))

