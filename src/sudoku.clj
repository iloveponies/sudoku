(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board coord]
  (let [[x y] coord
        y-values (range 0 9)]
    (set  (map #(value-at board [x %]) y-values))))

(defn col-values [board coord]
  (let [[x y] coord
        x-values (range 0 9)]
    (set (map #(value-at board [% y]) x-values))))

(defn coord-pairs [coord-sequence]
  (for [x coord-sequence
        y coord-sequence]
    [x y]))

(defn block-coords [board coord]
  (let [[x y] coord
        block-x (* (int (/ x 3)) 3)
        block-y (* (int (/ y 3)) 3)]
    (for [x (range block-x (+ 3 block-x))
          y (range block-y (+ 3 block-y))]
      [x y])))

(defn block-values [board coord]
  (set (map #(value-at board %)
            (block-coords board coord))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [row-vals (row-values board coord)
          col-vals (col-values board coord)
          block-vals (block-values board coord)
          existing-vals (set/union row-vals
                              col-vals
                              block-vals)]
      (set/difference all-values existing-vals))))

(defn all-vals [board]
  (let [rows (range 0 9)]
    (flatten (map #(get board %) rows))))

(defn filled? [board]
  (let [allvals (set (all-vals board))]
    (not (contains? allvals 0))))

(defn rows [board]
  (map #(set %) board))

(defn transpose [m]
  (apply mapv vector m))

(defn valid-rows? [board]
  (every? #(= all-values %)
          (rows board)))

(defn cols [board]
  (map #(set %) (transpose board)))

(defn valid-cols? [board]
  (every? #(= all-values %)
          (cols board)))

(def block-corners
  (for [x [0 1 2]
        y [0 1 2]]
    [(* 3 x) (* 3 y)]))

(defn blocks [board]
  (map #(block-values board %)
       block-corners))

(defn valid-blocks? [board]
  (every? #(= all-values %)
          (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(def all-coords
  (for [x (range 0 9)
        y (range 0 9)]
    [x y]))

(defn find-empty-point [board]
  (first (filter #(not (has-value? board %)) all-coords)))

(defn solve-helper [board]
  (if (valid-solution? board)
    [board]
    (let [point (find-empty-point board)
          choices (valid-values-for board point)]
      (for [choice choices
            solution (solve-helper (set-value-at board point choice))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))
