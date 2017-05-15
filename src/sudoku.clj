(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def worlds-hardest-board ;; according to: http://www.telegraph.co.uk/news/science/science-news/9359579/Worlds-hardest-sudoku-can-you-crack-it.html
  (board [[8 0 0 0 0 0 0 0 0]
          [0 0 3 6 0 0 0 0 0]
          [0 7 0 0 9 0 2 0 0]
          [0 5 0 0 0 7 0 0 0]
          [0 0 0 0 4 5 7 0 0]
          [0 0 0 1 0 0 0 3 0]
          [0 0 1 0 0 0 0 6 8]
          [0 0 8 5 0 0 0 1 0]
          [0 9 0 0 0 0 4 0 0]]))

;Ex. 1

(defn value-at [board coord]
  (let [[row col] coord]
    (get-in board [row col])))


;Ex. 2

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))


;Ex. 3

(defn row-values [board coord]
  (let [[row _] coord]
   (into #{} (nth board row))))


;Ex. 4

(defn col-values [board coord]
  (let [[_ col] coord]
    (into #{} (map #(nth % col) board))))


;Ex. 5

(defn coord-pairs [coord-sequence]
  (for [x coord-sequence y coord-sequence] [x y]))


;Ex: 6

; Helpers:
; Use defs here as they are evaluated only once.

(def all-values #{1 2 3 4 5 6 7 8 9})

(def coord->block
  (into {}
    (for [y (range 9) x (range 9)]
      (vector [x y] (+ (* (quot x 3) 3) (quot y 3))))))

(def block->coordlist
  (into {}
    (reduce (fn [dict [coord b]] (assoc dict b (cons coord (get dict b)))) {} coord->block)))

(defn coord->coordlist [coord]
  (-> coord coord->block block->coordlist))

(defn block-values [board coord]
  (set (map #(value-at board %) (coord->coordlist coord))))


;Ex: 7

(defn valid-values-for [board coord]
  (if (not (zero? (value-at board coord)))
    #{}
    (set/difference all-values (set/union (block-values board coord) (row-values board coord) (col-values board coord)))))


;Ex: 8

(defn filled? [board]
  (not (contains? (reduce (fn [s row] (set/union s (set row))) #{} board) 0)))


;Ex: 9

(defn rows [board]
  (apply vector (map #(into #{} %) board)))


(defn cols [board]
  (apply vector (for [col (range 9)]
    (set (map #(nth % col) board)))))


;Ex: 10

(defn blocks [board]
  (apply vector (for [coord block->coordlist]
    (set (map (partial value-at board) (nth coord 1))))))


;Ex: 11

(defn valid-rows? [board]
  (every? #(= all-values (set %)) (rows board)))

(defn valid-cols? [board]
  (every? #(= all-values (set %)) (cols board)))

(defn valid-blocks? [board]
  (every? #(= all-values (set %)) (blocks board)))


; Ex: 12

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))


; Ex: 13

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))


; Ex: 14

(defn find-empty-point [board]
  (reduce
    (fn [result coord]
      (if (zero? (get-in board coord))
        (reduced coord)))
    nil
    (for [row (range 9) col (range 9)] [row col])))


; Ex: 15

(defn get-move-list
  "Helper to generate a list of new board positions"
  [board]
  (let [next-coord (find-empty-point board)]
    (if next-coord
      (map #(set-value-at board next-coord %) (valid-values-for board next-coord))
      (list))))


(defn solve [board]
  (loop [board       board
         move-list   (list)]
    (if (valid-solution? board)
      board
      (let [new-move-list (lazy-cat (get-move-list board) move-list)]   ;; lazy-cat prevents stack overflow
        (if (empty? new-move-list)
          :unsolvable
          (recur (first new-move-list) (rest new-move-list))
        )))))

;(solve worlds-hardest-board)
