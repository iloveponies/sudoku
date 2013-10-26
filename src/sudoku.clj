(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (== 0 (value-at board coord))))

(defn row-values [board coord]
  (let [[fst _] coord
        row (get board fst)]
    (set row)))

(defn col-values [board coord]
  (let [[_ snd] coord
        get-snd-elem (fn [row]
                       (get row snd))]
    (set (map get-snd-elem board))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    (vector row col)))

(defn top-left-corner [coords]
  (let [[fst snd] coords
        row (- fst (mod fst 3))
        col (- snd (mod snd 3))]
    (vector row col)))

(defn block-values [board coord]
  (let [[x1 y1] (top-left-corner coord)
        vec-sum (fn [coord2]
                  (let [[x2 y2] coord2]
                   [(+ x1 x2) (+ y1 y2)]))
        block-coords (map vec-sum (coord-pairs [0 1 2]))
        value-at (fn [coord3]
                   (get-in board coord3))]
    (set (map value-at block-coords))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                    (set/union (row-values board coord)
                               (col-values board coord)
                               (block-values board coord)))))

(defn filled? [board]
  (not (contains? (set (apply concat board)) 0)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (empty? (disj (set (rows board))
                all-values)))

(defn cols [board]
  (for [x (range 0 9)]
    (col-values board [0 x])))

(defn valid-cols? [board]
  (empty? (disj (set (cols board))
                all-values)))

(defn blocks [board]
  (for [x [0 3 6]
        y [0 3 6]]
    (block-values board [x y])))

(defn valid-blocks? [board]
  (empty? (disj (set (blocks board))
                all-values)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [x 0
         y 0]
    (cond
     (and (has-value? board [x y])
          (< y 8)) (recur x (inc y))
     (and (has-value? board [x y])
          (== y 8)
          (< x 8)) (recur (inc x) 0)
     (not (has-value? board (vector x y))) [x y]
     :else nil)))

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      '())
    (let [point (find-empty-point board)
          valids (valid-values-for board point)]
      (for [n valids
            solution (solve-helper
                      (set-value-at board
                                    point
                                    n))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))





