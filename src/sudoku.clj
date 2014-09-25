(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= (value-at board coord) 0)))

(defn row-values [board [row col]]
  (set (get board row)))

(defn col-values [board [row col]]
  (let [helper  (fn [acc elem]
                  (conj acc (get elem col)))]
  (reduce helper #{} board)))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-values-helper [[row col]]
  [(* (int (/ row 3)) 3) (* (int (/ col 3)) 3)])

(defn block-values [board coord]
  (let [coord-corner (block-values-helper coord)
        coords-init  (coord-pairs (range 0 3))
        coords       (for [[x y] coords-init]
                        [(+ x (get coord-corner 0)) (+ y (get coord-corner 1))])
        mapper       (fn [elem] (value-at board elem))]
    (set (map mapper coords))))

(defn valid-values-for [board coord]
  (if (or (has-value? board coord) (nil? coord))
    #{}
    (set/difference
      (set/difference
        (set/difference
          all-values
          (block-values board coord))
        (row-values board coord))
      (col-values board coord))))

(defn all-board-values [board]
  (let [helper  (fn [acc elem]
                  (concat acc elem))]
    (reduce helper () board)))

(defn filled? [board]
  (= (count (filter zero? (all-board-values board))) 0))

(defn rows [board]
  (map set board))

(defn pred-all-values? [elem]
  (= elem all-values))

(defn valid-rows? [board]
    (every? pred-all-values? (rows board)))

(defn cols [board]
  (let [transposed-board  (apply map vector board)]
    (map set transposed-board)))

(defn valid-cols? [board]
  (every? pred-all-values? (cols board)))

(defn blocks [board]
  (let [coords-init  (coord-pairs (range 0 9 3))
        all-block-corners (map block-values-helper coords-init)
        mapper  (fn [elem] (block-values board elem))]
    (map mapper all-block-corners)))

(defn valid-blocks? [board]
  (every? pred-all-values? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [coords  (for [x (range 0 9)
                      y (range 0 9)]
                  [x y])
        mapper  (fn [coord] [coord (value-at board coord)])
        zero-helper? (fn [[coord value]] (zero? value))
        zipped  (map mapper coords)
        candidate (get
                    (first (filter zero-helper? zipped))
                    0)]

    candidate))

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      [])
    (for [new-value (valid-values-for board (find-empty-point board))
          solution (solve-helper (set-value-at board (find-empty-point board) new-value))]
      solution)))

(defn solve [board]
  (first (solve-helper board)))

