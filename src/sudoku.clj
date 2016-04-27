(ns sudoku
  (:require [clojure.set :as set]))

(defn curry [f & args] (fn [& more-args] (apply f (concat args more-args))))

(def board identity)

(defn value-at [board [row column]]
  (get-in board [row column]))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [row _]]
  (set (for [col (range 9)]
         (value-at board [row col]))))

(defn col-values [board [_ column]]
  (set (for [row (range 9)]
         (value-at board [row column]))))

(defn coord-pairs [coords]
  (apply concat (map (fn [c] (for [n coords] [c n])) coords)))


;;     0 1 2   3 4 5   6 7 8
;; ------------------------
;; 0 | 5 3 0   0 7 0   0 0 0
;; 1 | 6 0 0   1 9 5   0 0 0
;; 2 | 0 9 8   0 0 0   0 6 0
;; 
;; 3 | 8 0 0   0 6 0   0 0 3
;; 4 | 4 0 0   8 0 3   0 0 1
;; 5 | 7 0 0   0 2 0   0 0 6
;; 
;; 6 | 0 6 0   0 0 0   2 8 0
;; 7 | 0 0 0   4 1 9   0 0 5
;; 8 | 0 0 0   0 8 0   0 7 9


(defn top-left [[row col]]
  (letfn [(align-3 [n] (- n (rem n 3)))]
    [(align-3 row) (align-3 col)]))

(defn block-coords [coord]
  (let [[rr cc] (top-left coord)]
    (for [r (range rr (+ rr 3))
          c (range cc (+ cc 3))]
      [r c])))

(defn block-values [board coord]
  (set (map (curry value-at board) (block-coords coord))))

(def valid-values #{1 2 3 4 5 6 7 8 9})

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
    (clojure.set/difference valid-values
                            (row-values   board coord)
                            (col-values   board coord)
                            (block-values board coord))))

(defn filled? [board]
  (every? (curry has-value? board)
          (for [r (range 9)
                c (range 9)]
            [r c])))

(defn rows [board]
  (for [r (range 9)]
    (row-values board [r 0])))

(defn cols [board]
  (for [col (range 9)]
    (col-values board [0 col])))

(defn valid-sets? [sets]
  (every? (fn [s] (empty? (clojure.set/difference valid-values s))) sets))

(defn valid-rows? [board]
  (valid-sets? (rows board)))

(defn valid-cols? [board]
  (valid-sets? (cols board)))

(defn blocks [board]
  (map (curry block-values board) 
       '([0 0] [0 3] [0 6]
         [3 0] [3 3] [3 6]
         [6 0] [6 3] [6 6])))

(defn valid-blocks? [board]
  (valid-sets? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn find-empty-point [board]
  (some (fn [c] (if (not (has-value? board c)) c)) (coord-pairs (range 9))))


(defn solve [board]
  (cond 
    (valid-solution? board) board
    (filled? board) false
    :else
      (let [p (find-empty-point board)]
        (some (fn [v] (solve (set-value-at board p v)))
              (valid-values-for board p)))))


