(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})
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
(def solved-board0
  (board [[5 3 4 6 7 8 9 1 2]
          [6 7 2 1 9 5 3 4 8]
          [1 9 8 3 4 2 5 6 7]
          [8 5 9 7 6 1 4 2 3]
          [4 2 6 8 5 3 7 9 1]
          [7 1 3 9 2 4 8 5 6]
          [9 6 1 5 3 7 2 8 4]
          [2 8 7 4 1 9 6 3 5]
          [3 4 5 2 8 6 1 7 9]]))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board coord]
  (let [[row col] coord]
    (set (board row))))

(defn col-values [board coord]
  (let [[row col] coord]
    (set (map #(%1 col) board))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-start [coord]
  (map #(* 3 (quot %1 3)) coord))

(defn block-values [board coord]
  (let [[row-start col-start] (block-start coord)]
    (set (for [row (range row-start (+ row-start 3))
               col  (range col-start (+ col-start 3))]
           (value-at board [row col])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values 
                    (row-values board coord)
                    (col-values board coord)
                    (block-values board coord))))

(defn filled? [board]
  (not (contains? (set (apply concat board)) 0)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? #(= all-values %1) (rows board)))

(defn cols [board]
  (map #(col-values board [0 %1]) (range 0 9)))

(defn valid-cols? [board]
  (every? #(= all-values %1) (cols board)))

(defn blocks [board]
  (map #(block-values board %1) (coord-pairs [0 3 6]) ))

(defn valid-blocks? [board]
  (every? #(= all-values %1) (blocks board)))

(defn valid-solution? [board]
  (reduce #(and %1 (%2 board)) [valid-rows? valid-cols? valid-blocks?]))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [coords (coord-pairs (range 0 9))]
    (cond 
     (empty? coords) '()
     (not (has-value? board (first coords))) (first coords)
     :else (recur (rest coords)))))

(defn solve0-helper [board]
  (cond (valid-solution? board) [board]
        (filled? board) '()
        :else (let  [next-point (find-empty-point board)
                     candidates (valid-values-for board next-point)] 
                (for [candidate candidates 
                      solution (solve0-helper 
                                (set-value-at board next-point candidate))]
                  solution))))

(defn solve0 [board]
  (first (solve0-helper board)))

(defn solve [board]
  (cond (valid-solution? board) board
        (filled? board) nil  
        :else (let [next-point (find-empty-point board)
                    candidates (valid-values-for board next-point) 
                    next-board #(set-value-at board next-point %1) 
                    boards (map next-board candidates)]
                (first (filter identity (map solve boards))))))
