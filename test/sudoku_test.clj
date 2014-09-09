(ns sudoku-test
  (:use iloveponies.tests.sudoku
        sudoku
        midje.sweet))

(fact "value-at takes a board and coords and returns the value at the given coord"
      :mine
      (let [board [[5 3 4 6 7 8 9 1 2]
                   [6 7 2 1 9 5 3 4 8]
                   [1 9 8 3 4 2 5 6 7]
                   [8 5 9 7 6 1 4 2 3]
                   [4 2 6 8 5 3 7 9 1]
                   [7 1 3 9 2 4 8 5 6]
                   [9 6 1 5 3 7 2 8 4]
                   [2 8 7 4 1 9 6 3 5]
                   [3 4 5 2 8 6 1 7 9]]]
        (value-at board [0 1]) => 3
        (value-at board [0 0]) => 5))

(facts "has-value? takes a board and coord and returns if 0 present at that coord"
       :mine
       (let [board [[5 3 0 0 7 0 0 0 0]
                    [6 0 0 1 9 5 0 0 0]
                    [0 9 8 0 0 0 0 6 0]
                    [8 0 0 0 6 0 0 0 3]
                    [4 0 0 8 0 3 0 0 1]
                    [7 0 0 0 2 0 0 0 6]
                    [0 6 0 0 0 0 2 8 0]
                    [0 0 0 4 1 9 0 0 5]
                    [0 0 0 0 8 0 0 7 9]]]
         (fact "has-value? returns false if value at coord is zero"
               (has-value? board [0 2]) => false)
         (fact "has-value? returns trus if value at coord is not zero"
               (has-value? board [1 0]) => true)))

(facts "row-values takes a board and coord and returns a set of the unique values in the row that coord is in."
       :mine
       (let [board [[5 3 0 0 7 0 0 0 0]
                    [6 0 0 1 9 5 0 0 0]
                    [0 9 8 0 0 0 0 6 0]
                    [8 0 0 0 6 0 0 0 3]
                    [4 0 0 8 0 3 0 0 1]
                    [7 0 0 0 2 0 0 0 6]
                    [0 6 0 0 0 0 2 8 0]
                    [0 0 0 4 1 9 0 0 5]
                    [0 0 0 0 8 0 0 7 9]]]
         (fact "row-values returns 0, 3, 5, 7 for row 0"
               (row-values board [0 2]) => #{0 3 5 7})
         (fact "row-values return 0, 6, 8, 9 for row 2"
               (row-values board [2 4]) => #{0 6 8 9})))

(facts "col-values takes a board and coord and returns a set of the unique values in the col that coord is in."
       :mine
       (let [board [[5 3 0 0 7 0 0 0 0]
                    [6 0 0 1 9 5 0 0 0]
                    [0 9 8 0 0 0 0 6 0]
                    [8 0 0 0 6 0 0 0 3]
                    [4 0 0 8 0 3 0 0 1]
                    [7 0 0 0 2 0 0 0 6]
                    [0 6 0 0 0 0 2 8 0]
                    [0 0 0 4 1 9 0 0 5]
                    [0 0 0 0 8 0 0 7 9]]]
         (fact "col-values returns 0, 8 for col 2"
               (col-values board [0 2]) => #{0 8})
         (fact "col-values return 3, 1, 6, 0, 5, 9 for col 8"
               (col-values board [4 8]) => #{3 1 6 0 5 9})))

(facts "coord-pairs returns all coordinate pairs of the form [row col] where row is from the coord-sequence argument and col is from the coord-sequence."
       :mine
       (fact "Coord pairs for [0 1] are [[0 0] [0 1] [1 0] [1 1]]"
             (coord-pairs [0 1]) => [[0 0] [0 1] [1 0] [1 1]])
       (fact "Coord pairs for [0 1 2] are [[0 0] [0 1] [0 2] [1 0] [1 1] [1 2] [2 0] [2 1] [2 2]] "
             (coord-pairs [0 1 2]) => [[0 0] [0 1] [0 2] [1 0] [1 1] [1 2] [2 0] [2 1] [2 2]]))

(facts "block-values returns a set with all numbers in the block of coordinates."
       :mine
       (let [board [[5 3 0 0 7 0 0 0 0]
                    [6 0 0 1 9 5 0 0 0]
                    [0 9 8 0 0 0 0 6 0]
                    [8 0 0 0 6 0 0 0 3]
                    [4 0 0 8 0 3 0 0 1]
                    [7 0 0 0 2 0 0 0 6]
                    [0 6 0 0 0 0 2 8 0]
                    [0 0 0 4 1 9 0 0 5]
                    [0 0 0 0 8 0 0 7 9]]] 
        (fact "block-values for [0 2] are 0, 5, 3, 6, 8, 9"
              (block-values board [0 2]) => #{0 5 3 6 8 9})
        (fact "block-values for [4 5] are 0, 6, 8, 3, 2"
              (block-values board [4 5]) => #{0 6 8 3 2})))

(facts "valid-values-for should return empty set if coord has valid value or a list of valid numbers for that coord if there's no valid value"
       :mine
       (let [board [[5 3 0 0 7 0 0 0 0]
                    [6 0 0 1 9 5 0 0 0]
                    [0 9 8 0 0 0 0 6 0]
                    [8 0 0 0 6 0 0 0 3]
                    [4 0 0 8 0 3 0 0 1]
                    [7 0 0 0 2 0 0 0 6]
                    [0 6 0 0 0 0 2 8 0]
                    [0 0 0 4 1 9 0 0 5]
                    [0 0 0 0 8 0 0 7 9]]]
        (fact "valid-values-for returns empty set for coord with a valid values"
              (valid-values-for board [0 0]) => #{})
        (fact "valid-values-for returns valid numbers for coord which is empty (i.e. 0)"
              (valid-values-for board [0 2]) => #{1 2 4}
              (valid-values-for board [2 0]) => #{1 2})))

(facts "filled? returns true if there are no empty squares on the board, otherwise it returns false"
       :mine
       (fact "filled? returns true as no empty squares"
             (let [board [[5 3 4 6 7 8 9 1 2]
                          [6 7 2 1 9 5 3 4 8]
                          [1 9 8 3 4 2 5 6 7]
                          [8 5 9 7 6 1 4 2 3]
                          [4 2 6 8 5 3 7 9 1]
                          [7 1 3 9 2 4 8 5 6]
                          [9 6 1 5 3 7 2 8 4]
                          [2 8 7 4 1 9 6 3 5]
                          [3 4 5 2 8 6 1 7 9]]]
               (filled? board) => true))
       (fact "filled? returns false as board has empty squares"
             (let [board [[5 3 0 0 7 0 0 0 0]
                          [6 0 0 1 9 5 0 0 0]
                          [0 9 8 0 0 0 0 6 0]
                          [8 0 0 0 6 0 0 0 3]
                          [4 0 0 8 0 3 0 0 1]
                          [7 0 0 0 2 0 0 0 6]
                          [0 6 0 0 0 0 2 8 0]
                          [0 0 0 4 1 9 0 0 5]
                          [0 0 0 0 8 0 0 7 9]]]
               (filled? board)) => false))

(facts "rows returns a sequence of value sets for each row of the board supplied."
       :mine
       (let [board [[5 3 0 0 7 0 0 0 0]
                    [6 0 0 1 9 5 0 0 0]
                    [0 9 8 0 0 0 0 6 0]
                    [8 0 0 0 6 0 0 0 3]
                    [4 0 0 8 0 3 0 0 1]
                    [7 0 0 0 2 0 0 0 6]
                    [0 6 0 0 0 0 2 8 0]
                    [0 0 0 4 1 9 0 0 5]
                    [0 0 0 0 8 0 0 7 9]]]
         (fact "rows returns a set of values from each row of the board."
               (rows board) => '( #{0 3 5 7}
                                  #{6 0 1 9 5}
                                  #{0 9 8 6}
                                  #{8 0 6 3}
                                  #{4 0 8 3 1}
                                  #{7 0 2 6}
                                  #{0 6 2 8}
                                  #{0 4 1 9 5}
                                  #{0 8 7 9}))))

(facts "cols returns a sequence of value sets for each col of the board supplied."
       :mine
       (let [board [[5 3 0 0 7 0 0 0 0]
                    [6 0 0 1 9 5 0 0 0]
                    [0 9 8 0 0 0 0 6 0]
                    [8 0 0 0 6 0 0 0 3]
                    [4 0 0 8 0 3 0 0 1]
                    [7 0 0 0 2 0 0 0 6]
                    [0 6 0 0 0 0 2 8 0]
                    [0 0 0 4 1 9 0 0 5]
                    [0 0 0 0 8 0 0 7 9]]]
         (fact "cols returns a set of values from each col of the board."
               (cols board) => '( #{5 6 0 8 4 7}
                                  #{3 0 9 6}
                                  #{0 8}
                                  #{0 1 8 4}
                                  #{7 9 0 6 2 1 8}
                                  #{0 5 3 9}
                                  #{0 2}
                                  #{0 6 8 7}
                                  #{0 3 1 6 5 9}))))

(facts "valid-rows? returns true if every row in the board is a valid filled row"
       :mine
       (let [board [[5 3 0 0 7 0 0 0 0]
                    [6 0 0 1 9 5 0 0 0]
                    [0 9 8 0 0 0 0 6 0]
                    [8 0 0 0 6 0 0 0 3]
                    [4 0 0 8 0 3 0 0 1]
                    [7 0 0 0 2 0 0 0 6]
                    [0 6 0 0 0 0 2 8 0]
                    [0 0 0 4 1 9 0 0 5]
                    [0 0 0 0 8 0 0 7 9]]]
         (fact "valid-rows? should return false as board has non-valid rows"
               (valid-rows? board) => false))
       (let [board [[5 3 4 6 7 8 9 1 2]
                          [6 7 2 1 9 5 3 4 8]
                          [1 9 8 3 4 2 5 6 7]
                          [8 5 9 7 6 1 4 2 3]
                          [4 2 6 8 5 3 7 9 1]
                          [7 1 3 9 2 4 8 5 6]
                          [9 6 1 5 3 7 2 8 4]
                          [2 8 7 4 1 9 6 3 5]
                          [3 4 5 2 8 6 1 7 9]]]
         (fact "valid-rows? should return true as all rows are valid."
               (valid-rows? board) => true)))
