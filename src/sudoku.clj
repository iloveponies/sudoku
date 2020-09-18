(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (get-in board coord))))

(defn row-values [board coord]
  (set (get board (get coord 0))))

(defn col-values [board coord]
  (set (map (fn [row] (get row (get coord 1))) board)))

(defn coord-pairs [coords]
  (for [row coords
        column coords]
    (vector row column)))

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
  (set (for [row (block-row coord)
             column (block-col coord)]
         (value-at board (vector row column)))))

(defn valid-values-for [board coord]
  (if (zero? (value-at board coord))
    (set/difference all-values
                    (set/union (row-values board coord)
                               (col-values board coord)
                               (block-values board coord)))
    #{}))

(defn filled? [board]
  (not (contains?
         (set (map (fn [coord]
                     (value-at board coord))
                   (coord-pairs (range 0 9))))
         0)))

(defn rows [board]
  (map (fn [row]
         (row-values board [row 0]))
       (range 0 9)))

(defn valid-rows? [board]
  (empty? (filter (fn [row]
            (not= all-values row))
          (rows board))))

(defn cols [board]
  (map (fn [column]
         (col-values board [0 column]))
       (range 0 9)))

(defn valid-cols? [board]
  (empty? (filter (fn [column]
            (not= all-values column))
          (cols board))))

(defn blocks [board]
  (for [row [0 3 6]
        column [0 3 6]]
    (block-values board (vector row column))))

(defn valid-blocks? [board]
  (empty? (filter (fn [block]
            (not= all-values block))
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
                 (for [row (range 0 9)
                       column (range 0 9)]
                   (vector row column)))))

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
