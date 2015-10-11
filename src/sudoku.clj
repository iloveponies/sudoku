(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (contains? all-values (value-at board coord)))

(defn row-values [board coord]
  (let [[y x] coord] 
    (into #{} (get board y))))

(defn col-values [board coord]
  (let [x (second coord)]
    (into #{}
      (for [y (range 9)]
        (get-in board [y x])))))

(defn coord-pairs [coords]
  (for [x coords y coords]
    [x y]))

(defn block-values [board coord]
  (let [min-y (* (int (/ (first coord) 3)) 3)
        max-y (+ min-y 3)
        min-x (* (int (/ (second coord) 3)) 3)
        max-x (+ min-x 3)]
    (into #{}
      (for [y (range min-y max-y)
            x (range min-x max-x)]
        (value-at board [y x])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/intersection
      (set/difference
        all-values
        (block-values board coord))
      (set/difference
        all-values
        (row-values board coord))
      (set/difference
        all-values
        (col-values board coord)))))

(defn filled? [board]
  (letfn [(row-filled? [row]
            (every? all-values row))]
    (every? true? (map row-filled? board))))

(defn rows [board]
  (for [y (range 9)]
    (into #{}
      (get board y))))

(defn valid-line? [line]
  (empty? (set/difference all-values line)))

(defn valid-rows? [board]
  (every? true? (map valid-line? (rows board))))

(defn cols [board]
  (for [x (range 9)]
    (into #{}
      (for [y (range 9)]
        (get-in board [y x])))))

(defn valid-cols? [board]
  (every? true? (map valid-line? (cols board))))

(defn blocks [board]
  (for [top-y [0 3 6] top-x [0 3 6]]
    (into #{} 
      (for [y [0 1 2] x [0 1 2]]
        (value-at board [(+ top-y y) (+ top-x x)])))))

(defn valid-blocks? [board]
  (every? true? (map valid-line? (blocks board))))

(defn valid-solution? [board]
  (and
    (valid-rows? board)
    (valid-cols? board)
    (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first
    (for [y (range 9), x (range 9)
          :when (not (has-value? board [y x]))]
      [y x])))

(defn solve [board]
  (if-let [empty-point (find-empty-point board)]
    (for [valid-value (valid-values-for board empty-point)
          solution (solve (set-value-at board empty-point valid-value))]
      solution)
    (when (valid-solution? board)
      board)))
