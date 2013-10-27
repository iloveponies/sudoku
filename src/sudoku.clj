(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def valid-values (set (range 1 10)))
(def coord-range (set (range 0 9)))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board [row col]]
  (set (get-in board [row])))

(defn col-values [board [row col]]
  (let [col-helper(fn [column-set board-row] (conj column-set (get board-row col))) ]
  (reduce col-helper #{} board)))

(defn coord-pairs [coords]
  (into [] (for [x coords
                 y coords]
             (vector x y))))

(defn block-corner [[row col]]
  (let [corner(fn[x] (-  x (rem x 3)))]
  [(corner row) (corner col)]))

(defn block-values [board coord]
  (let [coord-range(fn[z] (range z (+ 3 z)))]
    (def corner (block-corner coord))
    (into #{} (for [row (coord-range (first corner))
                    col (coord-range (second corner))]
                (value-at board [row col])))))

(defn valid-values-for [board coord]
  (def coord-val (value-at board coord))
  (if (not= 0 coord-val)
    #{}
    (set/difference valid-values (set/union (block-values board coord) (row-values board coord) (col-values board coord)))))

(defn board-values [board]
  (let [helper(fn [board-values row] (apply conj board-values row))]
    (reduce helper #{} board)))

(defn filled? [board]
  (not (contains? (board-values board) 0)))

(defn rows [board]
  (let [helper(fn [result row] (conj result (set row)))]
  (reduce helper [] board)))

(defn valid-content? [board-contents]
  (loop [board-contents board-contents]
    (cond
      (empty? board-contents) true
      (not= valid-values (first board-contents)) false
      :else (recur (rest board-contents)))))

(defn valid-rows? [board]
  (valid-content? (rows board)))

(defn cols [board]
  (into [] (for [col coord-range]
              (col-values board [0 col]))))

(defn valid-cols? [board]
  (valid-content? (cols board)))

(defn blocks [board]
  (into [] (for [row (range 0 7 3)
                 col (range 0 7 3)]
             (block-values board [row col]))))

(defn valid-blocks? [board]
  (valid-content? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  nil)

(defn solve [board]
  nil)

