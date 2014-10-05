(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

;; (def sudoku-board
;;   (board [[5 3 0 0 7 0 0 0 0]
;;           [6 0 0 1 9 5 0 0 0]
;;           [0 9 8 0 0 0 0 6 0]
;;           [8 0 0 0 6 0 0 0 3]
;;           [4 0 0 8 0 3 0 0 1]
;;           [7 0 0 0 2 0 0 0 6]
;;           [0 6 0 0 0 0 2 8 0]
;;           [0 0 0 4 1 9 0 0 5]
;;           [0 0 0 0 8 0 0 7 9]]))

;; (def solved-board
;;   (board [[5 3 4 6 7 8 9 1 2]
;;           [6 7 2 1 9 5 3 4 8]
;;           [1 9 8 3 4 2 5 6 7]
;;           [8 5 9 7 6 1 4 2 3]
;;           [4 2 6 8 5 3 7 9 1]
;;           [7 1 3 9 2 4 8 5 6]
;;           [9 6 1 5 3 7 2 8 4]
;;           [2 8 7 4 1 9 6 3 5]
;;           [3 4 5 2 8 6 1 7 9]]))

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board [r _]]
  (set (map
        #(value-at board [r %])
        (range 9))))

(defn col-values [board [_ c]]
  (set (map
        #(value-at board [% c])
        (range 9))))

(defn coord-pairs [coords]
  (for [r coords
        c coords]
    [r c]))

(defn top-left [[r c]]
  [(- r (mod r 3)) (- c (mod c 3))])

(defn block-values [board coord]
  (let [tl (top-left coord)
        block-coords (map #(map + tl %) (coord-pairs [0 1 2]))]
    (set (map #(value-at board %) block-coords))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference
     all-values
     (set/union
      (row-values board coord)
      (col-values board coord)
      (block-values board coord)))))

(defn filled? [board]
  (let [all-values (for [r (range 9)
                         c (range 9)]
                     (value-at board [r c]))]
    (not (contains? (set all-values) 0))))

(defn rows [board]
  (map (fn [r] (row-values board [r 0])) (range 9)))


(defn valid-rows? [board]
  (every? #(empty? (set/difference all-values %)) (rows board)))


(defn cols [board]
  (map (fn [c] (col-values board [0 c])) (range 9)))

(defn valid-cols? [board]
  (every? #(empty? (set/difference all-values %)) (cols board)))

(defn blocks [board]
  (for [r (range 0 9 3)
        c (range 0 9 3)]
    (block-values board [r c])))

(defn valid-blocks? [board]
  (every? #(empty? (set/difference all-values %)) (blocks board)))

(defn valid-solution? [board]
  (and
   (valid-rows? board)
   (valid-cols? board)
   (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter
          #(not (has-value? board %))
          (for [r (range 9)
                c (range 9)]
            [r c ]))))

(defn solve-helper [current-board]
  (if (filled? current-board)
    (if (valid-solution? current-board) [current-board] [])
    (let [empty-position (find-empty-point current-board)]
      (for [valid-value-for-point (valid-values-for current-board empty-position)
            solution (solve-helper (set-value-at current-board empty-position valid-value-for-point))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))
