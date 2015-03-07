(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values ( set[1 2 3 4 5 6 7 8 9]))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (= (value-at board coord) 0) false true))

(defn row-values [board coord]
  (let [y (first coord)]
    (set (get board y))))

(defn col-values [board coord]
  (let [x (second coord)
        gety (fn[a, row] (conj a (get row x)))]
   (set (reduce gety () board) )))

(defn coord-pairs [coords]
  (vec (for [x coords
        y coords]
      [x y])))

(defn block-values [board coord]
  (let [tly (int (- (first coord) (mod (first coord) 3)))
        tlx (int (- (second coord) (mod (second coord) 3)) )]
   (set (for [y (range tly (+ tly 3))
          x (range tlx (+ tlx 3))]
      (value-at board [y x]))
    )))

(defn valid-values-for [board coord]
  (let [row (row-values board coord)
        col (col-values board coord)
        block (block-values board coord)]
  (if (has-value? board coord) (set ())
    (set/difference all-values (set/union row (set/union col block))))))

(defn filled? [board]
 (if (contains? (set (for [y [0 1 2 3 4 5 6 7 8]
    x [0 1 2 3 4 5 6 7 8]]
    (value-at board [y x]))) 0)
   false
   true))

(defn rows [board]
  (for [y [0 1 2 3 4 5 6 7 8]]
    (row-values board [y 0 ])))

(defn valid-rows? [board]
  (if (empty? (disj (set (rows board)) all-values)) true false))

(defn cols [board]
  (for [x [0 1 2 3 4 5 6 7 8]]
    (col-values board [0 x])))

(defn valid-cols? [board]
  (if (empty? (disj (set (cols board)) all-values)) true false))

(defn blocks [board]
  (for [x [0 3 6]
        y [0 3 6]]
    (block-values board [x y])))

(defn valid-blocks? [board]
  (if (empty? (disj (set (blocks board)) all-values)) true false))

(defn valid-solution? [board]
  (and (valid-blocks? board) (valid-cols? board) (valid-rows? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [seq (coord-pairs all-values)]
    (if (empty? seq) nil
      (if ((complement has-value?) board (first seq))
        (first seq)
        (recur (rest seq))))))
(defn solve [board]
  nil)
