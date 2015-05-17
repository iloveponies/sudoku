(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (zero? (value-at board coord)) false true))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (set (map (fn [x] (get x col)) board))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
        [row col]))

(defn block-values [board coord]
  (let [[a b] coord
        aBl (- a (mod a 3))
        bBl (- b (mod b 3))]
    (set (for [x (range aBl (+ aBl 3))
               y (range bBl (+ bBl 3))]
           (value-at board [x y])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
  (clojure.set/difference 
    all-values 
    (set/union 
      (row-values board coord) 
      (col-values board coord) 
      (block-values board coord)))))


(defn filled? [board]
 (not (some zero? (flatten board))))

(defn rows [board]
  (for [a (range 0 9)]
    (row-values board [a 0])))

(defn valid-rows? [board]
  (every? true? (map (fn [x] (= all-values x)) (rows board))))

(defn cols [board]
  (for [b (range 0 9)]
    (col-values board [0 b])))

(defn valid-cols? [board]
  (every? true? (map (fn [x] (= all-values x)) (cols board))))

(defn blocks [board]
  (for [a (range 0 9 3)
        b (range 0 9 3)]
        (block-values board [a b])))

(defn valid-blocks? [board]
  (every? true? (map (fn [x] (= all-values x)) (blocks board))))

(defn valid-solution? [board]
  (if (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)) true false))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (fn [x] (zero? (value-at board x))) (coord-pairs (range 0 9)))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      '())
    (let [e (find-empty-point board)]
      (for [v (valid-values-for board e)
            s (solve (set-value-at board e v))]
        s))))
