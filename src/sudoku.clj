(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[row _] coord]
    (loop [index 0
           a-set #{}]
      (if (== index 9)
        a-set
        (recur (inc index)
               (conj a-set (value-at board [row index])))))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (loop [index 0
           a-set #{}]
      (if (== index 9)
        a-set
        (recur (inc index)
               (conj a-set (value-at board [index col])))))))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-start [coord]
  (let [[x y] coord]
    [(* (int (/ x 3)) 3) (* (int (/ y 3)) 3)]))

(defn block-values [board coord]
  (let [[bx by] (block-start coord)]
    (set (for [x [0 1 2]
               y [0 1 2]]
           (value-at board [(+ bx x) (+ by y)])))))


(defn valid-values-for [board coord]
  (if (zero? (value-at board coord))
    (set/difference all-values
                    (row-values board coord)
                    (col-values board coord)
                    (block-values board coord))
    #{}))

(defn board-values [board]
  (loop [nboard board
         a-set #{}]
    (if (empty? nboard)
      (set a-set)
      (recur (rest nboard) (concat a-set (first nboard))))))

(defn filled? [board]
  (not (contains? (board-values board) 0)))

(defn rows [board]
  (for [x all-values]
    (set (for [y all-values]
           (value-at board [(- x 1) (- y 1)])))))

(defn valid-rows? [board]
  (every? (fn [x] (= x all-values)) (rows board)))

(defn cols [board]
  (for [y all-values]
    (set (for [x all-values]
           (value-at board [(- x 1) (- y 1)])))))

(defn valid-cols? [board]
  (every? (fn [x] (= x all-values)) (cols board)))

(defn blocks [board]
  (for [x [0 3 6]
        y [0 3 6]]
      (block-values board [x y])))

(defn valid-blocks? [board]
  (every? (fn [x] (= x all-values)) (cols board)))

(defn valid-solution? [board]
  (cond
   (not (filled? board)) false
   (not (valid-rows? board)) false
   (not (valid-cols? board)) false
   (not (valid-blocks? board)) false
   :else true))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter not-empty (for [x all-values
                                 y all-values]
                             (if (zero? (value-at board [(- x 1) (- y 1)]))
                               [(- x 1) (- y 1)]
                               [])))))

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      [])
    (let [[x y] (find-empty-point board)]
      (for [v (valid-values-for board [x y])
            solution (solve-helper (set-value-at board [x y] v))]
        solution))))

(defn solve [board]
  (solve-helper board))
