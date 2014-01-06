(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board coord]
  (let [[row col] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[row col] coord]
    (set (map (fn [x] (value-at board [x col])) (range 9)))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    (vector row col)))

(defn block-values-helper [board a-seq row-3-block]
  (apply concat (for [x a-seq]
    (take 3 (nthrest (board x) (* row-3-block 3))))))

(defn block-values [board coord]
  (let [[row col] coord
    ; (int (/ 0 3)) ;=> 0  (int (/ 3 3)) ;=> 1  (int (/ 6 3)) ;=> 2
        y-pos-3-block (int (/ row 3))
        row-3-block (int (/ col 3))]
        (cond
          (= y-pos-3-block 0) (set (block-values-helper board (range 3) row-3-block))
          (= y-pos-3-block 1) (set (block-values-helper board (range 3 6) row-3-block))
          :else               (set (block-values-helper board (range 6 9) row-3-block)))))

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn valid-values-for [board coord]
  (cond
    (has-value? board coord) '#{}
    :else (set/difference all-values (block-values board coord) (row-values board coord) (col-values board coord))))

(defn filled-helper [board row col]
  (let [values (row-values board [row col])
        n (count values)]
    (cond
      (or (> 9 n) (contains? values 0)) false
      (= row 8) (and (= 9 n) (not= contains? values 0))
      :else (filled-helper board (+ 1 row) col))))

(defn filled? [board]
  (filled-helper board 0 0))

(defn rows [board]
  (map (fn [x] (row-values board [x 0])) (range 9)))

(defn valid-rows? [board]
  ; filled-helper already checks each row for exactly same conditions
  (filled-helper board 0 0))

(defn cols [board]
  (map (fn [x] (col-values board [0 x])) (range 9)))

(defn valid-cols?-helper [board col]
  (let [values (col-values board [0 col])
        n (count values)]
    (cond
      (or (> 9 n) (contains? values 0)) false
      (= col 8) (and (= 9 n) (not= contains? values 0))
      :else (valid-cols?-helper board (+ 1 col)))))

(defn valid-cols? [board]
  (valid-cols?-helper board 0))

(defn blocks [board]
  (map (fn [x] (block-values board x)) (coord-pairs (range 1 9 3))))

(defn valid-blocks?-helper [board blocks i]
  (let [values (first blocks)
        n (count (set values))]
    (cond
      (or (> 9 n) (contains? values 0)) false
      (= i 8) (and (= 9 n) (not= contains? values 0)) 
      :else (valid-blocks?-helper board (rest blocks) (+ 1 i)))))
  

(defn valid-blocks? [board]
  (valid-blocks?-helper board (blocks board) 0))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn all-coords [n]
  (for [x (range n)
        y (range n)]
        (vector x y)))

(defn find-empty-point-helper [board coords]
  (cond
    (= 0 (get-in board (first coords))) (first coords)
    (empty? coords) nil
    :else (find-empty-point-helper board (rest coords))))

(defn find-empty-point [board]
  (find-empty-point-helper board (all-coords 9)))

(defn solve-helper [board]
  (if (valid-solution? board)
    board
    (let [empty-point (find-empty-point board)
          remaining (valid-values-for board empty-point)]
      (for [elem remaining
        solution (solve-helper (set-value-at board empty-point elem))]
        solution))))

(defn solve [board]
  (solve-helper board))
