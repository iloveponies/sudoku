(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (zero? (value-at board coord)) false true))

(defn row-values [board coord]
  (let [[x y] coord]
    (set (get board x))))

(defn col-values [board coord]
  (let [[x y] coord]
    (set (map (fn [row] (get row y)) board))))

(defn coord-pairs [coords]
  (for [given-coords coords
        same-coords coords]
    (vector given-coords same-coords)))

(defn top-left [board coord]
  (let [[given-x given-y] coord
        [top-left-x top-left-y] [(- given-x (rem given-x 3)) (- given-y (rem given-y 3))]]
    [top-left-x top-left-y]))

;(defn manual-block-coordinates [coord]
;  (let [[x y] coord]
;    [[x y]       [x (+ 1 y)]        [x (+ 2 y)]
;     [(+ 1 x) y] [(+ 1 x) (+ 1 y)]  [(+ 1 x) (+ 2 y)]
;     [(+ 2 x) y] [(+ 2 x) (+ 1 y)]  [(+ 2 x) (+ 2 y)]]))

(defn block-coordinates [top-left]
  (let [[x y] top-left]
    (for [x-range (range 3)
          y-range (range 3)]
      [(+ x x-range) (+ y y-range)])))

(defn block-values [board coord]
  (let [calculated-coord (top-left board coord)]
    (set (map (fn [coord] (value-at board coord)) (block-coordinates calculated-coord)))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [unified-set (set/union (row-values board coord)
                                 (col-values board coord)
                                 (block-values board coord))]
      (set/difference all-values unified-set))))

(defn set-board-all-values [board]
  (apply set/union (map (fn [x-coord]
                    (row-values board [x-coord 0])) (range (count (first board))))))

(defn filled? [board]
  (not (contains? (set-board-all-values board) 0)))

(defn rows [board]
  (map (fn [x-coord]
         (row-values board [x-coord 0])) (range (count (first board)))))

(defn values-validity [f board]
  (reduce set/union
          #{}
          (map  (fn [block] (set/difference all-values block)) (f board))))

(defn valid-rows? [board]
  (empty? (values-validity rows board)))

(defn cols [board]
  (map (fn [y-coord]
         (col-values board [0 y-coord])) (range (count (first board)))))

(defn valid-cols? [board]
  (empty? (values-validity cols board)))


(defn all-top-left-coordinates []
  (for [x-range [0 3 6]
        y-range [0 3 6]]
    [x-range y-range]))

;(defn manual-all-top-left-coordinates []
;  [[0 0] [0 3] [0 6]
;   [3 0] [3 3] [3 6]
;   [6 0] [6 3] [6 6]])

(defn blocks [board]
  (map (fn [[x-coord y-coord]]
         (block-values board [x-coord y-coord])) (all-top-left-coordinates)))

(defn valid-blocks? [board]
  (empty? (values-validity blocks board)))

(defn valid-solution? [board]
  (empty? (set/difference (set/union #{(valid-rows? board)}
                                     #{(valid-cols? board)}
                                     #{(valid-blocks? board)}) #{true})))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (for [x (range 9)
               y (range 9)
               :when (not (has-value? board [x y]))]
           [x y])))


(defn remaining-values-for-empty-point [board coord]
  (clojure.set/difference all-values (clojure.set/union (block-values board coord) (row-values board coord) (col-values board coord))))

(defn solution-helper [board]
  (if (valid-solution? board)
    [board]
    (let [empty-point (find-empty-point board)
          remaining (remaining-values-for-empty-point board empty-point)]
      (for [elem remaining
            solution (solution-helper (set-value-at board empty-point elem))]
        solution))))

(defn solve [board]
   (first (solution-helper board)))
