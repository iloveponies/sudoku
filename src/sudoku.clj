(ns sudoku
  (:require [clojure.set :as set]))

(def all-values #{1 2 3 4 5 6 7 8 9})

(def board identity)

(def value-at get-in)

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [x y]]
  (set (get board x)))

(defn col-values [board [x y]]
  (set (map (fn [a-seq] (get a-seq y)) board)))

(defn coord-pairs [coords]
  (for [coord1 coords coord2 coords]
    [coord1 coord2]))

(defn block-coords [[x y]]
  [(* 3 (int (/ x 3))) (* 3 (int (/ y 3)))])

(defn block-values [board coord]
  (let [[x y] (block-coords coord)]
    (set (apply concat (map (fn [x] (subvec (get board x) y (+ y 3)))
                            (range x (+ x 3)))))))

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
    (set/difference all-values (block-values board coord) (row-values board coord) (col-values board coord))))

(defn board-values [board]
  (apply concat (map vec board)))

(defn filled? [board]
  (not (contains? (set (board-values board)) 0)))

(defn rows [board]
  (vec (map (fn [y] (row-values board [y 0])) (range 0 9))))

(defn valid-rows? [board]
  (if (filled? board)
    (every? true? (map (fn [row] (= row all-values)) (rows board)))
    false))

(defn cols [board]
  (vec (map (fn [x] (col-values board [0 x])) (range 0 9))))

(defn valid-cols? [board]
  (if (filled? board)
    (every? true? (map (fn [col] (= col all-values)) (cols board)))
    false))

(defn blocks [board]
  (vec (map (fn [x] (block-values board [(* 3 (int (/ x 3))) (* 3 (mod x 3)) ])) (range 0 9))))

(defn valid-blocks? [board]
  (if (filled? board)
    (every? true? (map (fn [block] (= block all-values)) (blocks board)))
    false))

(defn valid-solution? [board]
  (every? true? [(valid-rows? board) (valid-cols? board) (valid-blocks? board)]))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [pos (loop [i 0 values (board-values board)]
    (cond
      (empty? values) nil
      (zero? (first values)) i
      :else (recur (inc i) (rest values))))]
    [(int (/ pos 9)) (mod pos 9)]))

(defn solve-helper [board]
  (if (filled? board) (if (valid-solution? board) [board] '())
    (let [point (find-empty-point board)]
      (for [number (valid-values-for board point)
            solution (solve-helper (set-value-at board point number))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))









