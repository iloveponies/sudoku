(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at
  [board coord]
  (get-in board coord))

(defn has-value?
  [board coord]
  (not (zero? (value-at board coord))))

(defn row-values
  [board [x _]]
  (set (get board x)))

(defn- transpose
  [matrix]
  (vec (apply map vector matrix)))

(defn col-values
  [board [_ y]]
  (set (get (transpose board) y)))

(defn coord-pairs
  [coords]
  (vec (for [x coords
             y coords]
         [x y])))

(defn- normalize-coord
  [n]
  (cond (< n 3) 0
        (< n 6) 3
        :else 6))

(defn- normalize-coords
  [coords]
  (mapv normalize-coord coords))
  
(defn block-values
  [board coord]
  (let [[x y] (normalize-coords coord)]
    (set (mapcat #(subvec % y (+ y 3))
                 (subvec board x (+ x 3))))))

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn valid-values-for
  [board coord]
  (if (has-value? board coord)
    #{}
    (let [used-values (clojure.set/union
                        (block-values board coord)
                        (row-values board coord)
                        (col-values board coord))]
      (clojure.set/difference all-values used-values))))

(defn filled?
  [board]
  (not (some zero? (flatten board))))

(defn rows
  [board]
  (mapv #(row-values board %)
        (for [x (range 9)]
          [x 0])))

(defn valid-rows?
  [board]
  (every? (partial = all-values) (rows board)))

(defn cols
  [board]
  (mapv #(col-values board %)
        (for [y (range 9)]
          [0 y])))


(defn valid-cols?
  [board]
  (every? (partial = all-values) (cols board)))

(comment
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
  )

(defn blocks
  [board]
  (mapv #(block-values board %)
        (for [x (range 0 9 3)
              y (range 0 9 3)]
          [x y])))

(defn valid-blocks?
  [board]
  (every? (partial = all-values) (blocks board)))

(defn valid-solution?
  [board]
  (every? true?
          [(valid-cols? board)
           (valid-rows? board)
           (valid-blocks? board)]))

(defn set-value-at
  [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point
  [board]
  (first
    (apply concat
           (map-indexed
             (fn [x col]
               (remove nil?
                       (map-indexed
                         (fn [y value]
                           (when (zero? value)
                             [x y]))
                         col)))
             board))))

(defn solve
  [board]
  (let [point (find-empty-point board)
        values (valid-values-for board point)]
    (mapcat (fn [value]
              (let [board (set-value-at board point value)]
                (cond (valid-solution? board) board
                      (filled? board) nil
                      :else (solve board))))
            values)))

