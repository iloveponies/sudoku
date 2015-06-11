(ns sudoku-test
  (:use iloveponies.tests.sudoku)
  (:use midje.sweet sudoku))

  (facts "block-range"
    (block-range [0 4]) => [[0 1 2] [3 4 5]]
    (block-range [5 5]) => [[3 4 5] [3 4 5]]
    (block-range [7 1]) => [[6 7 8] [0 1 2]])
