(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[r _] coord
        row (get board r)]
    (set row)))

(defn col-values [board coord]
  (let [[_ c] coord
        col (map #(get % c) board)]
    (set col)))

(defn coord-pairs [coords]
  (vec (for [row coords
             col coords]
         [row col])))

(defn top-left [coord]
  (let [[r c] coord]
    [(* 3 (int (/ r 3)))
     (* 3 (int (/ c 3)))]))

(defn block-values [board coord]
  (let [[r c] coord
        [tr tc] [(* 3 (int (/ r 3)))
                 (* 3 (int (/ c 3)))]
        block (for [i [0 1 2]
                    j [0 1 2]]
                (value-at board [(+ tr i) (+ tc j)]))]
    (set block)))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference
     all-values
     (set/union (row-values board coord)
                (col-values board coord)
                (block-values board coord)))))

(defn filled? [board]
  (let [board-values
        (for [r (range 9)
              c (range 9)]
          (value-at board [r c]))]
    (not (contains? (set board-values) 0))))

(defn rows [board]
  (vec (for [r (range 9)]
         (row-values board [r 0]))))

(defn valid-rows? [board]
  (let [valid-row? (fn [valid? row] (= row all-values))]
    (reduce valid-row? true (rows board))))

(defn cols [board]
  (vec (for [c (range 9)]
         (col-values board [0 c]))))

(defn valid-cols? [board]
  (let [valid-col? (fn [valid? col] (= col all-values))]
    (reduce valid-col? true (cols board))))

(defn blocks [board]
  (vec (for [r [0 3 6]
             c [0 3 6]]
         (block-values board [r c]))))

(defn valid-blocks? [board]
  (let [valid-block? (fn [valid? block] (= block all-values))]
    (reduce valid-block? true (blocks board))))

(defn valid-solution? [board]
  (and
   (valid-rows? board)
   (valid-cols? board)
   (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [empty-coords (for [r (range 9)
                           c (range 9)]
                       (if (not (has-value? board [r c]))
                         [r c]))]
    (first (remove nil? empty-coords))))

(defn solve [board]
  (vec (if (filled? board)
         (if (valid-solution? board)
           board
           '())
         (let [empty-location (find-empty-point board)]
           (for [valid-value (valid-values-for board empty-location)
                 solution (solve (set-value-at board empty-location valid-value))]
             solution)))))

