(ns sudoku-generator
  (:require [clojure.set :as set])
  (:use [sudoku]))


(def sudoku-board
  (board [[5 3 4 6 7 8 9 1 2]
          [6 7 2 1 9 5 3 4 8]
          [1 9 8 3 4 2 5 6 7]
          [8 5 9 7 6 1 4 2 3]
          [4 2 6 8 5 3 7 9 1]
          [7 1 3 9 2 4 8 5 6]
          [9 6 1 5 3 7 2 8 4]
          [2 8 7 4 1 9 6 3 5]
          [3 4 5 2 8 6 1 7 9]]))

(defn cycle-n-rounds [row]
  (let [n (* board-size board-size)
        x (+ 11 (rand-int 20))
        cycled  (take n (drop x (cycle row)))]
    (zipmap row cycled)))

(defn swap [a-board]
  (let [size (* board-size board-size)
        coords (coord-pairs (range size))
        a-map (cycle-n-rounds (first a-board))]
    (reduce #(let [board %1
                   index %2
                   current (value-at a-board index)]
               (set-value-at board index (get a-map current)))
            a-board
            coords)))

(defn not-single-solution? [a-solution]
  (let [size (* board-size board-size)]
    (not (nil? (nth a-solution size nil)))))

(defn generate-problem [board]
  (let [size (* board-size board-size)
        x (rand-int size)
        y (rand-int size)
        index [x y]
        new-board (set-value-at board index 0)
        a-solution (solve new-board)]
    (if (or (not-single-solution? a-solution)
            (empty? a-solution))
        board
        (generate-problem new-board))))

(defn transpose [a-board]
  (into [] (apply map vector a-board)))

(defn generate-random-problem []
  (generate-problem 
   (transpose  
    (first (repeat (rand-int 10) (swap sudoku-board))))))


