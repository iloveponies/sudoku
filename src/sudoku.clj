(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= (get-in board coord) 0)))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (reduce
    (fn [a b]
      (conj a (value-at board [b (second coord)])))
    #{}
    (range 9)))

(defn coord-pairs [coords]
  (for [y coords
        x coords]
    [y x]))

(defn top-left-corner [coord]
  (vector
    (* 3 (int (/ (first coord) 3)))
    (* 3 (int (/ (second coord) 3)))))

(defn coord-pairs-in-block [coord]
  (map
    (fn [coord-pair]
      [(+ (first coord-pair) (first (top-left-corner coord)))
       (+ (second coord-pair) (second (top-left-corner coord)))])
    (coord-pairs [0 1 2])))

(defn block-values [board coord]
  (reduce
    (fn [result-set coords-vec]
      (conj result-set (value-at board coords-vec)))
    #{}
    (coord-pairs-in-block coord)))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference
      all-values
      (set/union
        (block-values board coord)
        (row-values board coord)
        (col-values board coord)))))

(defn filled? [board]
  (empty?
    (set/difference
      (set
        (for [y (range 9) x (range 9)]
        (value-at board [y x])))
      all-values)))

(defn rows [board]
  (into []
    (for [y (range 9)]
      (set (row-values board [y 0])))))

(defn valid-rows? [board]
  (every?
    (fn [row]
      (empty? (set/difference all-values row)))
    (rows board)))


(defn cols [board]
  (into []
    (for [x (range 9)]
      (set (col-values board [0 x])))))

(defn valid-cols? [board]
  (every?
    (fn [col]
      (empty? (set/difference all-values col)))
  (cols board)))

(defn blocks [board]
  (into []
    (for [y (range 3)
          x (range 3)]
      (set
        (block-values
          board
          [(* 3 y) (* 3 x)])))))

(defn valid-blocks? [board]
  (every?
    (fn [block]
      (empty? (set/difference all-values block)))
  (blocks board)))

(defn valid-solution? [board]
  (and
    (valid-rows? board)
    (valid-cols? board)
    (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [all-coords (coord-pairs (range 9))]
    (when all-coords
      (if (zero? (value-at board (first all-coords)))
        (first all-coords)
        (recur (next all-coords))))))

(defn solve [board]
  nil)
