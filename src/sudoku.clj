(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board [(first coord) (second coord)]))

(defn has-value? [board coord]
  (not (= (value-at board coord) 0)))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (let [helper (fn [new b]
                 (conj new (get b (second coord))))]
    (reduce helper #{} board)))

(defn coord-pairs [coords]
  (for [a coords
        b coords]
    [a b]))

(defn block-values [board coord]
  (let [row0 (* (int (/ (first coord) 3)) 3)
        col0 (* (int (/ (second coord) 3)) 3)]
    (set (for [r [0 1 2]
               c [0 1 2]]
           (value-at board [(+ row0 r) (+ col0 c)])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [used-vals (set/union
                    (row-values board coord)
                    (col-values board coord)
                    (block-values board coord))]
      (set/difference all-values used-vals))))

(defn filled? [board]
  (let [all-vals (set (apply concat board))]
    (not (contains? all-vals 0))))

(defn rows [board]
  (for [row (range 0 9)]
    (row-values board [row 0])))

(defn valid-rows? [board]
  (every? (fn [x] (= x all-values)) (rows board)))

(defn cols [board]
  (for [col (range 0 9)]
    (col-values board [0 col])))

(defn valid-cols? [board]
  (every? (fn [x] (= x all-values)) (cols board)))

(defn blocks [board]
  (for [block (coord-pairs [0 3 6])]
    (block-values board block)))

(defn valid-blocks? [board]
  (every? (fn [x] (= x all-values)) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (fn [x] (= 0 (value-at board x))) (coord-pairs (range 0 9)))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      [])
    (let [empty-coord (find-empty-point board)
          valid-vals (valid-values-for board empty-coord)]
      (for [x valid-vals
            solution (solve (set-value-at board empty-coord x))]
        solution))))
