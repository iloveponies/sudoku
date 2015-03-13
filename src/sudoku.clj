(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def block-size 3)

(def board-size (int (Math/pow block-size 2)))

(def board-coord-range (range 0 board-size))

(def all-values (into #{} (range 1 (inc board-size))))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  ((complement zero?) (value-at board coord)))

(defn row-values [board coord]
  (into #{} (get board (first coord))))

(defn col-values [board coord]
  (into #{} (map (fn [row] (get row (second coord))) board)))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-values [board coord]
  (let [top-left-x (* (int (/ (first coord) block-size)) block-size)
        top-left-y (* (int (/ (second coord) block-size)) block-size)]
    (into #{} (for [x (range top-left-x (+ top-left-x 3))
                    y (range top-left-y (+ top-left-y 3))]
                (value-at board [x y])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [used-values (set/union (row-values board coord) (col-values board coord) (block-values board coord))]
      (set/difference all-values used-values))))

(defn filled? [board]
  (let [all-coord (for [x board-coord-range
                        y board-coord-range]
                    [x y])]
    (every? (fn [coord] (has-value? board coord)) all-coord)))

(defn rows [board]
  (for [x board-coord-range]
    (row-values board [x 0])))

(defn valid-rows? [board]
  (every? (fn [row] (= row all-values)) (rows board)))

(defn cols [board]
  (for [y board-coord-range]
    (col-values board [0 y])))

(defn valid-cols? [board]
  (every? (fn [col] (= col all-values)) (cols board)))

(defn blocks [board]
  (for [x (range 0 board-size block-size)
        y (range 0 board-size block-size)]
    (block-values board [x y])))

(defn valid-blocks? [board]
  (every? (fn [block] (= block all-values)) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [all-coord (for [x board-coord-range
                        y board-coord-range]
                    [x y])]
    (first (filter (fn [coord] (not (has-value? board coord))) all-coord))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      (lazy-seq nil))
    (let [empty-point (find-empty-point board)
          possible-values (valid-values-for board empty-point)]
      (for [next-possible-value possible-values
            solution (solve (set-value-at board empty-point next-possible-value))]
        solution))))
