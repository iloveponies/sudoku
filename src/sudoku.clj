(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (> (value-at board coord) 0))

(defn row-values [board [row _]]
  (set (board row)))

(defn col-values [board [_ y]]
  (set (map #(nth % y) board)))

(defn coord-pairs [coords]
  (vec (apply concat (for [n coords]
                       (for [i coords]
                         [n i])))))

(defn top-leftmost-coord-of-block [[row col]]
  [(- row (mod row 3)) (- col (mod col 3))])

(defn block-values [board coord]
  (let [[row col] (top-leftmost-coord-of-block coord)]
    (->> board
         (map (partial drop col))
         (map (partial take 3))
         (drop row)
         (take 3)
         flatten
         set)))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (clojure.set/difference
      all-values
      (block-values board coord)
      (row-values board coord)
      (col-values board coord))))

(defn filled? [board]
  (not (contains? (set (apply concat board)) 0)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? empty? (map (partial clojure.set/difference all-values) (rows board))))

(defn cols [board]
  (for [n (range 0 9)]
    (col-values board [nil n])))

(defn valid-cols? [board]
  (every? empty? (map (partial clojure.set/difference all-values) (cols board))))

(defn blocks [board]
  (mapv (partial block-values board) [[0 0] [0 3] [0 6]
                                      [3 0] [3 3] [3 6]
                                      [6 0] [6 3] [6 6]]))

(defn valid-blocks? [board]
  (every? empty? (map (partial clojure.set/difference all-values) (blocks board))))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (for [coord (coord-pairs (range 0 9))
               :let [val (value-at board coord)]
               :when (zero? val)]
           coord)))

(defn solve [board]
  (if-let [empty-point (find-empty-point board)]
    (loop [vals (valid-values-for board empty-point)]
      (when (seq vals)
        (let [val (first vals)
              a-solution (solve (set-value-at board empty-point val))]
          (if (valid-solution? a-solution)
            a-solution
            (recur (rest vals))))))
    (when (valid-solution? board)
      board)))