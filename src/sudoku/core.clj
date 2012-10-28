(ns sudoku.core
  (:require [clojure.set :as set]))

;   | 0 1 2 | 3 4 5 | 6 7 8 |  
; --+-------+-------+-------+--
; 0 |       |       |       | 0
; 1 |       |       |       | 1
; 2 |       |       |       | 2
; --+-------+-------+-------+--
; 3 |       |       |       | 3
; 4 |       |       |       | 4
; 5 |       |       |       | 5
; --+-------+-------+-------+--
; 6 |       |       |       | 6
; 7 |       |       |       | 7
; 8 |       |       |       | 8
; --+-------+-------+-------+--
;   | 0 1 2 | 3 4 5 | 6 7 8 |  

(def board identity)
(def all-values (set (range 1 10)))

(def value-at get-in)

(defn has-value? [board coord]
  ((complement zero?) (value-at board coord)))

(defn row-values [board [x _]]
  (set (get board x)))

(defn col-values [board [_ y]]
  (set (map #(get % y) board)))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-values [board [x y]]
  (let [blocknum   (fn [c] (* (int (/ c 3)) 3))
        [bx by]    [(blocknum x) (blocknum y)]
        move-block (fn [[x y]] [(+ x bx) (+ y by)])
        cells      (map move-block (coord-pairs (range 3)))]
    (set (map #(value-at board %) cells))))

(defn valid-values-for [board coord]
  (let [row (row-values board coord)
        col (col-values board coord)
        blk (block-values board coord)
        used-values (set/union row col blk)]
    (reduce (fn [valid used] (disj valid used)) all-values used-values)))

(defn filled? [board]
  (every? true? (map #(has-value? board %) (coord-pairs (range 9)))))

(defn rows [board]
  (map #(row-values board %) (map #(vector % 0) (range 9))))

(defn has-all-values? [sets]
  (every? empty? (map #(set/difference all-values %) sets)))

(defn valid-rows? [board]
  (has-all-values? (rows board)))

(defn cols [board]
  (map #(col-values board %) (map #(vector 0 %) (range 9))))

(defn valid-cols? [board]
  (has-all-values? (cols board)))

(defn blocks [board]
  (map #(block-values board %) (coord-pairs (range 0 9 3))))

(defn valid-blocks? [board]
  (has-all-values? (blocks board)))

(def valid-solution? (every-pred valid-rows? valid-cols? valid-blocks?))

(def set-value-at assoc-in)

(defn find-empty-point [board]
  (loop [coords (coord-pairs (range 9))]
    (let [coord (first coords)]
      (cond
        (empty? coords)                nil
        (zero? (value-at board coord)) coord
        :else                          (recur (rest coords))))))

(defn solve-helper [board]
  (if (valid-solution? board)
    [board]
    (let [coord (find-empty-point board)]
      (for [value (valid-values-for board coord)
            solution (solve-helper (set-value-at board coord value))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))
