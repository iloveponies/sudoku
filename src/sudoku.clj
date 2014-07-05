(ns sudoku
  (:require [clojure.set :as set]))

(def all-values #{1 2 3 4 5 6 7 8 9})

(def board identity)

(def my-sudoku-board
  (board [[0 0 0 0 0 0 0 7 0]
          [0 6 9 0 3 0 0 0 2]
          [0 0 0 6 9 0 3 0 0]
          [0 0 3 0 0 9 0 0 4]
          [0 7 0 0 0 0 0 5 0]
          [1 0 0 8 0 0 2 0 0]
          [0 0 6 0 1 4 0 0 0]
          [3 0 0 0 7 0 6 8 0]
          [0 5 0 0 0 0 0 0 0]]))

#_(def solved-board
  (board [[5 3 4 6 7 8 9 1 2]
          [6 7 2 1 9 5 3 4 8]
          [1 9 8 3 4 2 5 6 7]
          [8 5 9 7 6 1 4 2 3]
          [4 2 6 8 5 3 7 9 1]
          [7 1 3 9 2 4 8 5 6]
          [9 6 1 5 3 7 2 8 4]
          [2 8 7 4 1 9 6 3 5]
          [3 4 5 2 8 6 1 7 9]]))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (pos? (value-at board coord)))

(defn row-values [board [x _]]
  (set(get board x)))

(defn col-values [board [_ y]]
  (set (map #(get % y) board)))

(defn coord-pairs [coords]
  (vec (for [x coords
             y coords]
         [x y])))

(defn block-top-left [[x y]]
  [(* 3 (quot x 3)) (* 3 (quot y 3))])

(defn block-values [board coord]
  (let [[t l] (block-top-left coord)
        block-coords (for [i (range t (+ t 3))
                           j (range l (+ l 3))]
                       [i j])]
    (set (map #(value-at board %) block-coords))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                    (set/union (row-values board coord)
                               (col-values board coord)
                               (block-values board coord)))))

(defn row-filled? [row]
  (not-any? zero? row))

(defn filled? [board]
  (not-any? false? (map row-filled? board)))

(defn board-numbers [board]
  (reduce concat board))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? #(empty? (set/difference all-values %)) (rows board)))

(defn cols [board]
  (map set (apply map
                  (fn [& all] all)
                  board)))

(defn valid-cols? [board]
  (every? #(empty? (set/difference all-values %)) (cols board)))

(def block-starts
  (for [i (range 3)
        j (range 3)]
    [(* i 3) (* j 3)]))

(defn block-values [board coord]
  (let [[x y] (block-top-left coord)]
    (set (for [i (range x (+ x 3))
               j (range y (+ y 3))]
           (value-at board [i j])))))

(defn blocks [board]
  (map set (map #(block-values board %) block-starts)))

(defn valid-blocks? [board]
  (every? #(empty? (set/difference all-values %)) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (for [i (range 9)
               j (range 9)
               :let [coord [i j]]
               :when (zero? (value-at board coord))]
           coord)))

(defn solve [board]
  (if (valid-solution? board)
    board
    (let [coord (find-empty-point board)
          valid-values (valid-values-for board coord)]
      (some #(solve (set-value-at board coord %)) valid-values))))
