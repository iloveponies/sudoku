(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [row _]]
  (reduce conj #{} (map (fn [col] (value-at board [row col])) (range 0 9))))

(defn col-values [board [_ col]]
  (reduce conj #{} (map (fn [row] (value-at board [row col])) (range 0 9))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-top-left [block-y block-x]
  [(* 3 block-y) (* 3 block-x)])

(defn print-board [board]
  (reduce (fn [rep row] (str rep "\n" row)) board))

(defn block-values [board [cell-y cell-x]]
  (let [block-y (int (/ cell-y 3))
        block-x (int (/ cell-x 3))
        cell-coords (for [y (range 0 3)
                                      x (range 0 3)]
                                  [(+ (* 3 block-y) y) (+ (* 3 block-x) x)])]
    (reduce conj #{} (map (fn [cell-coord] (value-at board cell-coord)) cell-coords))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (clojure.set/difference all-values
                            (block-values board coord)
                            (row-values board coord)
                            (col-values board coord))))

(defn filled? [board]
  (empty? (filter zero? (flatten board))))

(defn rows [board]
  (map (fn [r] (row-values board [r 0])) (range 0 9)))

(defn is-all-values? [a-set]
  (= all-values a-set))

(defn valid-rows? [board]
  (= 9 (count (filter is-all-values? (rows  board)))))

(defn cols [board]
  (map (fn [c] (col-values board [0 c])) (range 0 9)))

(defn valid-cols? [board]
  (= 9 (count (filter is-all-values? (cols board)))))

(defn blocks [board]
  (for [y (range 0 9 3) x (range 0 9 3)]
    (block-values board [y x])))

(defn valid-blocks? [board]
  (= 9 (count (filter is-all-values? (blocks board)))))

(defn valid-solution? [board]
  (and
   (valid-rows? board)
   (valid-cols? board)
   (valid-blocks? board)))

(defn set-value-at [board [row col] new-value]
  (assoc board row (assoc (get board row) col new-value)))

(defn find-empty-point [board]
  (let [coords (for [y (range 0 9) x (range 0 9)]
                 [y y])]
    (loop [candidates coords]
       (cond
          (empty? candidates) nil
          (not (has-value? board (first candidates))) (first candidates)
          :else (recur (rest candidates))))))

(defn solve [board]
  nil)
