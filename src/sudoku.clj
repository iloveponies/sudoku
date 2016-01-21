(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board [r c]]
  (reduce conj #{} (get board r)))

(defn col-values [board [r c]]
  (reduce (fn [acc row] (conj acc (get row c))) #{} board))

(defn coord-pairs [coords]
  (for [r coords
        c coords]
    [r c]))

(defn block-top-left [[r c]]
  [(- r (mod r 3)) (- c (mod c 3))])

(defn block-values [board coord]
  (let [[top left] (block-top-left coord)
        block-coords (for [r [top (+ 1 top) (+ 2 top)]
                           c [left (+ 1 left) (+ 2 left)]]
                      [r c])]
    (reduce conj #{} (map (fn [x] (value-at board x)) block-coords))))



(defn valid-values-for [board coord]
  (cond
   (has-value? board coord) #{}
   :else (let [assigned-values (set/union (block-values board coord) (row-values board coord) (col-values board coord))]
           (set/difference all-values assigned-values))))

(defn board-values [board]
    (for [coord (coord-pairs (range 9))]
      (value-at board coord)))

(defn filled? [board]
  (not (contains? (set (board-values board)) 0)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? identity (map (fn [row] (= row all-values)) (rows board))))

(defn cols [board]
  (for [c [0 1 2 3 4 5 6 7 8]]
    (col-values board [0 c])))

(defn valid-cols? [board]
  (every? identity (map (fn [col] (= col all-values)) (cols board))))

(defn blocks [board]
  (for [coord (coord-pairs [0 3 6])]
    (block-values board coord)))

(defn valid-blocks? [board]
  (every? identity (map (fn [block] (= block all-values)) (blocks board))))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (fn [point] (not (has-value? board point))) (coord-pairs (range 9)))))

(defn solve-helper [board]
  (cond
   (filled? board) (if (valid-solution? board) [board] [])
   :else (let [empty-point (find-empty-point board)]
           (for [value (valid-values-for board empty-point)
                 solution (solve-helper (set-value-at board empty-point value))]
             solution))))


(defn solve [board]
  (first (solve-helper board)))
