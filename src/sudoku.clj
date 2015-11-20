(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def sudoku-board0
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

(def sudoku-solved0
  (board [[0 3 4 6 7 8 9 1 2]
          [0 7 2 1 9 5 3 4 0]
          [0 9 8 3 4 2 5 6 7]
          [8 5 9 7 6 1 4 2 3]
          [4 2 6 8 5 3 7 9 1]
          [7 1 3 9 2 4 8 5 6]
          [9 6 1 5 3 7 2 8 4]
          [2 8 7 4 1 9 6 3 5]
          [3 4 5 2 8 6 1 7 0]]))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= (value-at board coord) 0))

(defn row-values [board coord]
  (set (value-at board [(first coord)])))

(defn col-values [board coord]
  (set (for [row (range 0 9)]
         (value-at board [row (second coord)]))))

(defn coord-pairs [coords]
  (for [i1 coords
        i2 coords]
    [i1 i2]))

(defn block-values [board coord]
  (let [row (first coord)
        col (second coord)
        block-row (* 3 (int (/ row 3)))
        block-col (* 3 (int (/ col 3)))]
    (set
      (for [row (range block-row (+ block-row 3))
            col (range block-col (+ block-col 3))]
       (value-at board [row col])))))

(defn valid-values-for [board coord]
  (if (= 0 (value-at board coord))
    (set/difference (set (range 1 10))
                    (set/union 
                       (row-values board coord)
                       (col-values board coord)
                       (block-values board coord)))
    #{}))

(defn filled? [board]
 (not (contains? (set (for [row (range 0 9)
                            col (range 0 9)]
                   (value-at board [row col])))
            0)))

(defn rows [board]
  (reduce 
    (fn [acc row] (conj acc (into #{} row)))
    []
    board))

(defn valid-row-col? [v]
  (and (not (contains? v 0))
       (= (count v) 9))) 

(defn valid-rows? [board]
  (nil? (some
          #(not (valid-row-col? %))
          (rows board))))

(defn cols [board]
  (reduce
    (fn [acc col] 
      (conj acc
        (into #{} 
          (for [row (range 0 9)]
            (value-at board [row col])))))
    []
    (range 0 9)))

(defn valid-cols? [board]
  (nil? (some
          #(not (valid-row-col? %))
          (cols board))))

(defn blocks [board]
  (for [row [0 3 6]
        col [0 3 6]]
    (block-values board [row col])))

(defn valid-blocks? [board]
  (nil? (some
          #(not (valid-row-col? %))
          (blocks board))))

(defn valid-solution? [board]
  (and
    (valid-rows?   board)
    (valid-cols?   board)
    (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (some #(if (= (value-at board %) 0) %) 
    (for [row (range 0 9)
          col (range 0 9)]
      [row col])))

(defn solve-helper [board]
  "Recursive Backtracking for solving Sudoku"
  (let [empty-point (find-empty-point board)]
    ;; if there is another empty point - solve for it
    (if empty-point
      (let [row (first  empty-point)
            col (second empty-point)]
        ;; find 'some' number in range [1, 10) that fits the current board
        (some 
          ;; 'some' either returns a solved board or nil
          (fn [i]
            ;; make a new board with a new value candidate
            (let [new-board (set-value-at board [row col] i)]
              ;; is the new board correct?
              (if (and
                    (not (contains? (row-values board [row col]) i))
                    (not (contains? (col-values board [row col]) i))
                    (not (contains? (block-values board [row col]) i)))
                 ;; true: recursively solve the new board
                 (let [new-board-solved (solve-helper new-board)]
                    ;; has this board been solved? 
                    (if new-board-solved
                      ;; true: return the solved board
                      new-board-solved))))) ;; false: return nil
          (range 1 10))) ;; 'some' returns either solved board or nil
       board))) ;; recursion complete: no empty points left

(defn solve [board]
  (or (solve-helper board) []))

