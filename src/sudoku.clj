(ns sudoku
  (:require [clojure.set :as set]))

(def all-values #{1 2 3 4 5 6 7 8 9})

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (== 0 (value-at board coord))))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (let [a (get coord 1)]
    (loop [acc 0 setti #{}]
      (if (== acc 9)
        setti
        (recur (inc acc) (conj setti (value-at board (vector acc a))))))))

(defn coord-pairs [coords]
  (for [row coords col coords]
    [row,col]))

(defn top-left-corner [row col]
  (let [help (fn [x]
               (cond
                (< x 3) 0
                (< x 6) 3
                :else 6))]
    [(help row), (help col)]))

(defn block-values [board coord]
  (let [[a b] (top-left-corner (get coord 0) (get coord 1))]
    (set (for [rows (range a (+ a 3))
               cols (range b (+ b 3))]
           (value-at board [rows cols])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference
     all-values
     (row-values board coord)
     (col-values board coord)
     (block-values board coord))))

(defn filled-helper [board coords]
  (cond
   (empty? coords) true
   (not (has-value? board (first coords))) false
   :else (filled-helper board (rest coords))))

(defn filled? [board]
  (let [a (coord-pairs [0 1 2 3 4 5 6 7 8])]
    (filled-helper board a)))

(defn rows [board]
  (loop [acc 0 setti []]
    (if (== acc 9)
      setti
      (recur (inc acc) (conj setti (row-values board [acc 0]))))))

(defn valid-rows? [board]
  (every? #(= all-values %) (rows board)))

(defn cols [board]
  (loop [acc 0 setti []]
    (if (== acc 9)
      setti
      (recur (inc acc) (conj setti (col-values board [0 acc]))))))

(defn valid-cols? [board]
  (every? #(= all-values %) (cols board)))

(defn blocks [board]
  (for [rows [0 3 6]
        cols [0 3 6]]
    (block-values board [rows cols])))

(defn valid-blocks? [board]
  (every? #(= all-values %) (cols board)))

(defn valid-solution? [board]
  (and
   (valid-cols? board)
   (valid-rows? board)
   (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first
   (filter #(not (has-value? board %))
           (for [row (range 9)
                 col (range 9)]
             [row col]))))
(defn solve [board]
  (cond
   (filled? board) (if (valid-solution? board)
                     board
                     [])
   :otherwise (let [empty-location (find-empty-point board)
                    valid-values (valid-values-for board empty-location)]
                (first (filter (complement empty?)
                               (for [values valid-values]
                                 (solve (set-value-at board
                                                      empty-location
                                                      values))))))))
