(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (> (value-at board coord) 0))

(defn row-values [board coord]
  (let [[row col] coord]
    (set (for [x (range 9)] (value-at board [row x])))))

(defn col-values [board coord]
  (let [[row col] coord]
    (set (for [x (range 9)] (value-at board [x col])))))

(defn coord-pairs [coords]
  (vec (for [x coords y coords] [x y])))

(defn top-left-of-block [coord]
  (let [[row col] coord]
    (vector (- row (int (mod row 3))) (- col (int (mod col 3))))))

(defn block-values [board coord]
  (let [[ox oy] (top-left-of-block coord)]
     (set (for [x (range 3) y (range 3)] (value-at board [(+ ox x) (+ oy y)])))))

(defn valid-values-for [board coord]
  (let [all-values #{1 2 3 4 5 6 7 8 9}]
  (if (has-value? board coord)
    #{}
  (clojure.set/difference all-values (clojure.set/union (col-values board coord) (row-values board coord) (block-values board coord))))))

(defn filled? [board]
  (nil? (some #{0} (set (for [x (range 9) y (range 9)] (value-at board [x y]))))))

(defn rows [board]
  (vec (map #(row-values board [% 0]) (range 9))))

(defn valid-rows? [board]
  (let [all-values #{1 2 3 4 5 6 7 8 9}]
    (every? true? (map #(= all-values %) (rows board)))))

(defn cols [board]
  (vec (map #(col-values board [0 %]) (range 9))))

(defn valid-cols? [board]
  (let [all-values #{1 2 3 4 5 6 7 8 9}]
     (every? true? (map #(= all-values %) (cols board)))))

(defn blocks [board]
  (vec (for [x (range 0 9 3) y (range 0 9 3)] (block-values board [x y]))))

(defn valid-blocks? [board]
  (let [all-values #{1 2 3 4 5 6 7 8 9}]
     (every? true? (map #(= all-values %) (blocks board)))))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
   (first (for [x (range 9) y (range 9) :let [v (value-at board [x y])] :when (= 0 v)] [x y])))

(defn solver-helper [board]
  (if (filled? board)
     (if (valid-solution? board)
       [board]
       ())
     (let [next-empty (find-empty-point board)]
        (for [v (valid-values-for board next-empty)
               solution (solver-helper (set-value-at board next-empty v))]
         solution))))

(defn solve [board]
  (first (solver-helper board)))
