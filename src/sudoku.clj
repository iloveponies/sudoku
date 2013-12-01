(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (> (value-at board coord) 0))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (set (for [row board]
         (get row (second coord)))))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    (vector x y)))

(defn block-values [board coord]
  (let [block-x (* (int (/ (first coord) 3)) 3)
        block-y (* (int (/ (second coord) 3)) 3)]
    (set (for [x (range block-x (+ block-x 3))
               y (range block-y (+ block-y 3))]
           (value-at board [x y])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference (set (range 10))
                            (set/union (row-values board coord)
                                               (col-values board coord)
                                               (block-values board coord)))))

(defn filled? [board]  
  (every? (fn [x] (not (contains? (set x) 0))) board))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? true? (for [row (rows board)]
                  (empty? (set/difference (set (range 1 10)) row)))))

(defn cols [board]
  (for [col (range 9)]  
    (col-values board [0 col])))

(defn valid-cols? [board]
  (every? true? (for [col (cols board)]
                  (empty? (set/difference (set (range 1 10)) col)))))

(defn blocks [board]
  (for [x (range 3)
        y (range 3)]
    (block-values board [(* x 3) (* y 3)])))

(defn valid-blocks? [board]
  (every? true? (for [block (blocks board)]
                  (empty? (set/difference (set (range 1 10)) block)))))

(defn valid-solution? [board]
  (and (and (valid-rows? board) (valid-cols? board)) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (complement nil?) (for [x (range 9)
                                         y (range 9)]
                                     (if (zero? (value-at board [x y]))
									  [x y])))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      ())
    (let [coord (find-empty-point board)]
      (for [value (valid-values-for board coord)
            solution (solve (set-value-at board coord value))]
        solution))))
