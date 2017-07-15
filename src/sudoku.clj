(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (zero? (value-at board coord))
    false
    true))

(defn row-values [board [row _]]
  (set (get board row)))

(defn col-values [board coord]
  (let [[_ y] coord]
    (set (map (fn [elem] (get elem y)) board))))

(defn coord-pairs [coords]
  (for [cr1 coords
        cr2 coords]
    [cr1 cr2]))

(defn block-values [board coord]
  (let [[x y] coord
        block-x-left (* 3 (int (/ x 3)))
        block-y-top (* 3 (int (/ y 3)))]
    (set (for [i (range block-x-left (+ block-x-left 3))
               j (range block-y-top (+ block-y-top 3))]
           (get-in board [i j])))))

(def possible-values #{1 2 3 4 5 6 7 8 9})

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference possible-values (set/union (block-values board coord)
                                             (row-values board coord)
                                             (col-values board coord)))))

(defn filled? [board]
  (not (contains? (reduce (fn [s row] (set/union s (set row))) #{} board) 0)))

(defn rows [board]
  (map (fn [row] (set row)) board))

(defn valid-rows? [board]
  (if (every? (fn [value] (if (= value possible-values)
                            true
                            false))
              (rows board))
    true
    false))

(defn cols [board]
  (for [y (range 0 9)]
    (set (for [x (range 0 9)]
           (value-at board [x y])))))

(defn valid-cols? [board]
  (if (every? (fn [value] (if (= value possible-values)
                            true
                            false))
              (cols board))
    true
    false))

(defn blocks [board]
  (for [x [0 3 6]
        y [0 3 6]]
    (block-values board [x y])))

(defn valid-blocks? [board]
  (if (every? (fn [value] (if (= value possible-values)
                            true
                            false))
              (blocks board))
    true
    false))

(defn valid-solution? [board]
  (if (and (valid-blocks? board) (valid-cols? board)
           valid-rows? board)
    true
    false))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (remove (fn [coord] (has-value? board coord)) (coord-pairs (range 9)))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      [])
    (let [coord (find-empty-point board)
          values (valid-values-for board coord)]
      (for [value values
            solution (solve (set-value-at board coord value))]
        solution))))
