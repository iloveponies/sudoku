(ns sudoku
  (:require [clojure.set :as set]))

;[[5 3 0 | 0 7 0 | 0 0 0]
; [6 0 0 | 1 9 5 | 0 0 0]
; [0 9 8 | 0 0 0 | 0 6 0]
; -------+-------+-------
; [8 0 0 | 0 6 0 | 0 0 3]
; [4 0 0 | 8 0 3 | 0 0 1]
; [7 0 0 | 0 2 0 | 0 0 6]
; -------+-------+-------
; [0 6 0 | 0 0 0 | 2 8 0]
; [0 0 0 | 4 1 9 | 0 0 5]
; [0 0 0 | 0 8 0 | 0 7 9]]

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[row _] coord] 
    (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (set (map #(get % col) board))))

(defn coord-pairs [coords]
  (for [x coords y coords] [x y]))

(defn get-block-coords [coord]
  (let [[x y] coord 
        row-index (quot x 3) 
        col-index (quot y 3)
        row-coords (range (* 3 row-index) (* 3 (inc row-index)))
        col-coords (range (* 3 col-index) (* 3 (inc col-index)))]
    (for [a row-coords b col-coords] [a b])))

(defn block-values [board coord]
  (->> (get-block-coords coord)
       (map #(get-in board %))
       (set)))

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
    (let [block-vals (block-values board coord)
          row-vals (row-values board coord)
          col-vals (col-values board coord)
          filled-vals (set/union block-vals row-vals col-vals)]
      (set/difference all-values filled-vals))))

(defn filled? [board]
  (->> board
       (map #(contains? (set %) 0))
       (every? false?)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? #(= % all-values) (rows board)))

(defn cols [board]
  (let [n (count (first board))] 
    (map (fn [i] 
           (set (map #(get % i) board))) 
         (range n))))

(defn valid-cols? [board]
  (every? #(= % all-values) (cols board)))

(defn blocks [board]
  (let [edge-coords (for [x [0 3 6] y [0 3 6]] [x y])]
    (map #(block-values board %) edge-coords)))

(defn valid-blocks? [board]
  (every? #(= % all-values) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [coords (for [x (range 9) y (range 9)] [x y])]
    (first (filter #(not (has-value? board %)) coords))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board) board [])
    (let [coord (find-empty-point board)
          solutions (map #(solve (set-value-at board coord %)) 
                         (valid-values-for board coord))]
        (first (filter (comp not empty?) solutions)))))
