(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= (value-at board coord) 0)))

(defn row-values [board coord]
  (let [[x y] coord]
    (set (get board x))))

(defn col-values [board coord]
  (let [[x y] coord]
  (set (map (fn [as] (get as y)) board))))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    (vec [x y])))

(defn block-values [board coord]
  (let [[x y] coord
        first-x (* 3 (int (/ x 3)))
        first-y(* 3 (int (/ y 3)))
        coords-x (range first-x (+ first-x 3))
        coords-y (range first-y (+ first-y 3))]
    (set (for [a coords-x b coords-y]
           (value-at board [a b])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                    (block-values board coord)
                    (row-values board coord)
                    (col-values board coord))))

(defn filled? [board]
  (let [two-vecs-1-board (set (apply concat board))]
    (not (contains? two-vecs-1-board 0))))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? true? (set (map (fn [x] (= x all-values)) (rows board)))))

(defn cols [board]
  (map (fn [x] (col-values board [0 x])) (range 9)))

(defn valid-cols? [board]
  (every? true? (set (map (fn [x] (= x all-values)) (cols board)))))

(defn blocks [board]
  (map (fn [x] (block-values board x)) (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (every? true? (set (map (fn [x] (= x all-values)) (blocks board)))))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (fn [x] (if (= (value-at board x) 0) x)) (coord-pairs (range 9)))))

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      '())
    (let [nxt-empty (find-empty-point board)]
      (for [valid-value (valid-values-for board nxt-empty)
            solution (solve-helper (set-value-at board nxt-empty valid-value))]
        solution))))

(defn solve [board]
  (solve-helper board))
