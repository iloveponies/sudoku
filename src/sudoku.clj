(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (= (value-at board coord) 0) false true))

(defn row-values [board coord]
  (let [x (first coord)
        row (get board x)]
    (set row)))

(defn col-values [board coord]
  (let [y (second coord)]
    (loop [cols board
           values nil]
      (if (empty? (first cols))
        (set values)
        (recur (rest cols) (conj values (get (first cols) y)))))))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    (vector x y)))

(defn top-left [board coord]
  (map (fn [i] (* (int (/ i 3)) 3)) coord))

(defn block-values [board coord]
  (let [top-left-coord (top-left board coord)
        x (first top-left-coord)
        y (second top-left-coord)
        x-range (range x (+ x 3))
        y-range (range y (+ y 3))
        values (for [x-value x-range
                     y-value y-range]
                 (value-at board (vector x-value y-value)))]
    (set values)))

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (clojure.set/difference all-values
                            (clojure.set/union (col-values board coord)
                                               (row-values board coord)
                                               (block-values board coord)))))

(defn filled? [board]
  (let [all-values (for [x (range 9)
                         y (range 9)]
                     (value-at board [x y]))]
    (if (some zero? all-values)
      false
      true)))

(defn rows [board]
  (for [x (range 9)]
    (row-values board [x 0])))

(defn valid? [x]
  (if (= x #{1 2 3 4 5 6 7 8 9}) true false))

(defn valid-rows? [board]
  (if (every? valid? (rows board)) true false))

(defn cols [board]
  (for [y (range 9)]
    (col-values board [0 y])))

(defn valid-cols? [board]
  (if (every? valid? (cols board)) true false))

(defn blocks [board]
  (for [x [0 3 6]
        y [0 3 6]]
    (block-values board [x y])))

(defn valid-blocks? [board]
  (if (every? valid? (blocks board)) true false))

(defn valid-solution? [board]
  (if (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)) true false))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (remove (fn [coord] (has-value? board coord)) (coord-pairs (range 9)))))

(defn solve [board]
  (if-let [point (find-empty-point board)]
    (let [valid-values (valid-values-for board point)]
      (for [value valid-values
            solution (solve (set-value-at board point value))]
        solution))
    (if (valid-solution? board)
      (first [board]))))
