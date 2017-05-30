(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board coord]
  (let [[x y] coord]
    (reduce (fn [a b] (conj a b)) #{} (board x))))

(defn col-values [board coord]
  (let [[x y] coord]
    (reduce (fn [a b] (conj a (get b y))) #{} board)))

(defn coord-pairs [coords]
  (for [c1 coords
        c2 coords]
    [c1 c2]
))

(defn top-left-coord [coords]
  (let [[r c] coords]
    [(- r (mod r 3)) (- c (mod c 3))]))

(defn block-values [board coord]
  (let [[x y] (top-left-coord coord)]
    (loop [theset #{} row x col y]
      (if (= 2 (mod row 3))
        (if (= 2 (mod col 3))
          (conj theset (value-at board [row col]))
          (recur (conj theset (value-at board [row col])) (- row 2) (+ col 1))
        )
        (recur (conj theset (value-at board [row col])) (+ row 1) col)))))


(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (reduce set/difference all-values
            [(row-values board coord) (col-values board coord) (block-values board coord)])))

(defn filled? [board]
  (not (contains? (set (reduce concat () board)) 0)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (reduce (fn [a b] (and a b))
  (map (fn [x] (empty? (set/difference all-values x))) (rows board))))

(defn cols [board]
  (for [r (range 0 9)]
    (reduce (fn [a b] (conj a (get b r))) #{} board)))

(defn valid-cols? [board]
  (reduce (fn [a b] (and a b))
  (map (fn [x] (empty? (set/difference all-values x))) (cols board))))

(defn blocks [board]
  (for [r [1 4 7]
        c [1 4 7]]
    (block-values board [r c])))

(defn valid-blocks? [board]
  (reduce (fn [a b] (and a b))
  (map (fn [x] (empty? (set/difference all-values x))) (blocks board))))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [row 0 col 0]
    (if (has-value? board [row col])
      (if (= 8 col)
        (if (= 8 row)
          nil
          (recur (inc row) 0)
        )
        (recur row (inc col))
      )
    [row col]
    )
  ))

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      ()
    )
    (let [epoint (find-empty-point board)]
      (for [solution (valid-values-for board epoint) rec (solve-helper (set-value-at board epoint solution))] rec))))

(defn solve [board]
  (first (solve-helper board)))

