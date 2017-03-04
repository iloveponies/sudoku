(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (> (value-at board coord) 0 ))

(defn row-values [board coord]
  (let [[x _] coord]
    (set (get board x))))

(defn col-values [board coord]
  (let [[_ y] coord]
    (reduce (fn [left right] (conj left (get right y))) #{} board)))

(defn coord-pairs [coords]
  (vec (for [a coords b coords] (vec [a b]))))

(defn block-findr [seq-x]
  (let[[x y] seq-x]
    (cond
      (and (< x 3) (< y 3))       [0 0]
      (and (< x 3) (<= 3 y 5))    [0 3]
      (and (< x 3) (<= 6 y))      [0 6]
      (and (<= 3 x 5) (< y 3))    [3 0]
      (and (<= 3 x 5) (<= 3 y 5)) [3 3]
      (and (<= 3 x 5) (>= y 6))   [3 6]
      (and (<= 6 x) (< y 3))      [6 0]
      (and (<= 6 x) (<= 3 y 5))   [6 3]
      (and (<= 6 x) (<= 6 y))     [6 6])))

(defn blocdinates [top-left-y]
  (let[[x y] top-left-y
        zerozero [[0 0] [0 1] [0 2] [1 0] [1 1] [1 2] [2 0] [2 1] [2 2]]]
          (for [number zerozero]
            (vec (conj ()
             (+ (first number) y)
             (+ (last number) x))))))

(defn block-values [board coord]
  (let [coordinates-for-block (blocdinates (block-findr coord))]
           (reduce (fn [left right] (conj left (value-at board right))) #{} coordinates-for-block)))

(defn valid-values-for [board coord]
  (if (> (value-at board coord) 0)
    #{}
    (let [blocks (block-values board coord)
          rows (row-values board coord)
          cols (col-values board coord)
          blocks-rows-cols (clojure.set/union blocks rows cols)]
            (clojure.set/difference all-values blocks-rows-cols))))

(defn filled? [board]
  (not (contains? (set (flatten board)) 0)))

(defn rows [board]
  (reduce (fn [l r] (conj l (set r))) [] board))

(defn cols [board]
  (reduce (fn [l r] (conj l (col-values board [0 (.indexOf board r)]))) [] board))

(defn blocks [board]
  (let [all-block-top-leftys [[0 0] [0 3] [0 6] [3 0] [3 3] [3 6] [6 0] [6 3] [6 6]]]
     (vec (for [bloc all-block-top-leftys]
       (block-values board bloc)))))

(defn count81? [list-a]
  (let [a (count (apply concat list-a))] (= 81 a)))

(defn valid-rows? [board]
  (count81? (rows board)))

(defn valid-cols? [board]
  (count81? (cols board)))

(defn valid-blocks? [board]
  (count81? (blocks board)))

(defn valid-solution? [board]
  (and
    (= 405 (reduce (fn [l r] (+ l r)) 0 (apply concat board)))
    (valid-rows? board)
    (valid-cols? board)
    (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (let[[x y] coord]
    (assoc-in board [x y] new-value)))

(defn find-empty-point [board]
  (let [x 0
        y 0
        finder (fn lookup [x y]
                 (if (= 0 (get-in board [x y]))
                       [x y]
                       (cond
                         (and (= x 8) (= y 8)) "probably solved"
                         (= y 8)  (lookup (inc x) 0)
                         :else (lookup x (inc y)))))]
    (finder x y)))

(defn helper-solver [board]
  (if (valid-solution? board)
    board
    (let [a (find-empty-point board)]
      (for [elem (valid-values-for board a)
            solution (helper-solver (set-value-at board a elem))]
              solution))))

(defn solve [board]
    (helper-solver board))
