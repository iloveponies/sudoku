(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (> (value-at board coord) 0))

(defn row-values [board [row _]]
  (set (get board row)))

(defn col-values [board [_ col]]
  (set (map (fn[x] (value-at board [x col])) (range 9))))

(defn coord-pairs [coords]
  (mapcat (fn[x] (map (fn[y] [x y]) coords)) coords))

(defn coord-pairs-xy [coordsx coordsy]
  (mapcat (fn[x] (map (fn[y] [x y]) coordsy)) coordsx))

(defn block-coords [[x1 y1]]
 (let [x (* (int (/ x1 3)) 3)
       y (* (int (/ y1 3)) 3)]
    (coord-pairs-xy (range x (+ x 3)) (range y (+ y 3)))
   ))

(defn block-values [board coord]
  (set (map (fn[x] (value-at board x)) (block-coords coord))))

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
    (let [invert-set (fn[x] (set/difference all-values x))]
      (invert-set
         (set/union
          (row-values board coord)
          (col-values board coord)
          (block-values board coord)
        )
      ))))

(defn filled? [board]
  (= (count(filter zero? (flatten board))) 0))

(defn rows [board]
  (map (fn[x] (row-values board [x 0])) (range 9)))

(defn valid-rows? [board]
  (= (count (filter (fn[x] (= (count (clojure.set/difference all-values x)) 0))
        (rows board))) 9))

(defn cols [board]
  (map (fn[x] (col-values board [0 x])) (range 9)))

(defn valid-cols? [board]
   (= (count (filter (fn[x] (= (count (clojure.set/difference all-values x)) 0))
        (cols board))) 9))

(defn blocks [board]
  (map (fn[x] (block-values board [x (* (mod x 3) 3)])) (range 9)))

(defn valid-blocks? [board]
  (= (count (filter (fn[x] (= (count (clojure.set/difference all-values x)) 0))
        (blocks board))) 9))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (fn[x] (zero? (value-at board x))) (coord-pairs (range 9)))))

(defn open-points [board]
  (filter (fn[x] (zero? (value-at board x))) (coord-pairs (range 9))))

(defn solve-helper [board]
  (if (filled? board)
    [board]
    (let [elem (find-empty-point board)]
        (for [valid-val (valid-values-for board elem)
              solution (solve-helper (set-value-at board elem valid-val))]
          solution
      ))))

(defn solve [board]
  (first (solve-helper board)))
