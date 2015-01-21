(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= (value-at board coord) 0))

(defn row-values [board [r c]]
  (set (get board r)))

(defn col-values [board [r c]]
  (set (map (fn [a-seq] (get a-seq c)) board)))

(defn coord-pairs [coords]
  (for [r coords
        c coords]
    [r c]))

(defn top-left [[r c]]
  [(- r (mod r 3)) (- c (mod c 3))])

(defn block-values [board coord]
  (let [[r c] (top-left coord)
        index-pairs (coord-pairs [0 1 2])
        values (for [[cr cc] index-pairs]
                 (value-at board [(+ r cr) (+ c cc)]))]
    (set values)))

(defn valid-values-for [board coord]
  (if (not= (value-at board coord) 0)
    #{}
    (let [used-values (set/union (row-values board coord)
                                 (col-values board coord)
                                 (block-values board coord))]
      (set/difference all-values used-values))))

(defn board-values [board]
  (set (apply concat board)))

(defn filled? [board]
  (not (contains? (board-values board) 0)))

(defn rows [board]
  (for [row board]
    (set row)))

(defn valid-rows? [board]
  (every? true? (for [a-row (rows board)]
                  (= (set/difference all-values a-row) #{}))))

(defn cols [board]
  (for [col (range 0 9)]
    (set (col-values board [0 col]))))

(defn valid-cols? [board]
  (every? true? (for [a-col (cols board)]
                  (= (set/difference all-values a-col) #{}))))

(defn blocks [board]
  (for [block-coords (coord-pairs [0 3 6])]
    (set (block-values board block-coords))))

(defn valid-blocks? [board]
  (every? true? (for [a-block (blocks board)]
                  (= (set/difference all-values a-block) #{}))))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (for [[x row] (map-indexed vector board)
        [y value] (map-indexed vector row)
        :when (zero? value)]
    [x y])))

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      [])
    (let [empty-point (find-empty-point board)]
      (for [value-candidate (valid-values-for board empty-point)
            solution (solve-helper (set-value-at board empty-point value-candidate))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))
