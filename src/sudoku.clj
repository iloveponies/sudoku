(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(def sudoku-board2
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

(def solved-board2
  (board [[5 3 4 6 7 8 9 1 2]
          [6 7 2 1 9 5 3 4 8]
          [1 9 8 3 4 2 5 6 7]
          [8 5 9 7 6 1 4 2 3]
          [4 2 6 8 5 3 7 9 1]
          [7 1 3 9 2 4 8 5 6]
          [9 6 1 5 3 7 2 8 4]
          [2 8 7 4 1 9 6 3 5]
          [3 4 5 2 8 6 1 7 9]]))

(def invalid-board2
  (board [[5 3 4 6 7 8 9 1 1]
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
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
 (set (get board (get coord 0))))

(defn cons-by-index [index seqs]
  (if (empty? seqs)
   '()
    (cons (first (drop index (first seqs))) (cons-by-index index (rest seqs)))))

(defn col-values [board coord]
  (set (cons-by-index (get coord 1) board)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    (conj (vector row) col)))


(defn block-top-left-coords [coord]
  (let [floor (fn [n]
                (cond
                 (<= 6 n) 6
                 (<= 3 n) 3
                 :else 0))]
    (vector (floor (get coord 0)) (floor (get coord 1)))))

; Spaghetti Enterprises

(defn block-values [board coord]
    (let [top-left (block-top-left-coords coord)]
      (set (map (fn [xy] (value-at board xy))
       (map
             (fn [[a b]] [(+ a (get top-left 0)) (+ b (get top-left 1))])
             (coord-pairs [0 1 2]))))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [get-outs-from-heres
          (set/union (col-values board coord)
                     (row-values board coord)
                     (block-values board coord))]
      (set/difference all-values get-outs-from-heres))))

(defn filled? [board]
  (not (contains? (set (flatten board)) 0)))

(defn rows [board]
  (map set board))

; row/col/block helper

(defn kikka13 [se]
  (let [diff (set/difference all-values se)]
    (or (= #{0} diff) (= #{} diff))))


(defn valid-rows? [board]
  (every? kikka13 (rows board)))


(defn cols [seqs]
    (let [length (count seqs)]
    (loop [index 0
           result '()]
      (if (== index length)
        (reverse result)
        (recur (inc index) (cons (set (cons-by-index index seqs)) result))))))

(defn valid-cols? [board]
  (every? kikka13 (cols board)))

(defn blocks [board]
  (loop [x 0
         y 0
         result []]
    (cond
     (== x y 6) (conj result (block-values board [x y]))
     (== x 6) (recur 0 (+ y 3) (conj result (block-values board [y x])))
     :else (recur (+ x 3) y (conj result (block-values board [y x]))))))



(defn valid-blocks? [board]
  (every? kikka13 (blocks board)))


(defn valid-solution? [board]
  (and
   (every? #(= all-values %) (rows board))
   (every? #(= all-values %) (cols board))
   (every? #(= all-values %) (blocks board))))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))



(defn find-empty-point [board]
  (loop [x 0
         y 0]
    (cond
     (> y 8) [-1 -1]
     (zero? (value-at board [x y])) [x y]
     (< x 8) (recur (inc x) y)
     :else (recur 0 (inc y)))))



(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      '())
    (let [try-coord (find-empty-point board)]
          (for [valid-value (valid-values-for board try-coord)
                solved (solve-helper (set-value-at board try-coord valid-value))]
            solved))))

(defn solve [board]
  (first (solve-helper board)))




























