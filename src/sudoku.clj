(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (get-in board coord))))

(defn row-values [board coord]
  (set (get board (get coord 0))))

(defn col-values [board coord]
  (set (map (fn [row] (get row (get coord 1))) board)))

(defn coord-pairs [coords]
  (for [num1 coords
        num2 coords]
    (vector num1 num2)))

(defn block-row [coord]
  (cond
    (contains? #{0 1 2} (get coord 0)) #{0 1 2}
    (contains? #{3 4 5} (get coord 0)) #{3 4 5}
    (contains? #{6 7 8} (get coord 0)) #{6 7 8}))

(defn block-col [coord]
  (cond
    (contains? #{0 1 2} (get coord 1)) #{0 1 2}
    (contains? #{3 4 5} (get coord 1)) #{3 4 5}
    (contains? #{6 7 8} (get coord 1)) #{6 7 8}))

(defn block-values [board coord]
  (set (for [num1 (block-row coord)
             num2 (block-col coord)]
         (value-at board (vector num1 num2)))))

(defn valid-values-for [board coord]
  (if (zero? (value-at board coord))
    (set/difference #{1 2 3 4 5 6 7 8 9}
                    (set/union (row-values board coord)
                               (col-values board coord)
                               (block-values board coord)))
    (set nil)))

(defn filled? [board]
  (not (contains?
         (set (map (fn [coord]
                     (value-at board coord))
                   (coord-pairs (range 0 9))))
         0)))

(defn rows [board]
  (map (fn [i]
         (row-values board [i 0]))
       (range 0 9)))

(defn valid-rows? [board]
  (empty? (filter (fn [row]
            (not= #{1 2 3 4 5 6 7 8 9} row))
          (rows board))))

(defn cols [board]
  (map (fn [i]
         (col-values board [0 i]))
       (range 0 9)))

(defn valid-cols? [board]
  (empty? (filter (fn [col]
            (not= #{1 2 3 4 5 6 7 8 9} col))
          (cols board))))

(defn blocks [board]
  (for [num1 [0 3 6]
        num2 [0 3 6]]
    (block-values board (vector num1 num2))))

(defn valid-blocks? [board]
  (empty? (filter (fn [block]
            (not= #{1 2 3 4 5 6 7 8 9} block))
          (blocks board))))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (fn [coord]
                   (zero? (value-at board coord)))
                 (for [num1 (range 0 9)
                       num2 (range 0 9)]
                   (vector num1 num2)))))

(defn board-solver [board solutions]
  (let [empty-point (find-empty-point board)]
    (if empty-point
      (for [elem (valid-values-for board empty-point)
            solution (board-solver
                       (set-value-at board empty-point elem)
                       solutions)]
        solution)
      (if (valid-solution? board)
        (conj solutions board)
        solutions))))

(defn solve [board]
  (first (board-solver board [])))
