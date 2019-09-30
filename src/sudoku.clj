(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (pos? (value-at board coord)))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (set (map (fn [row] (get row col)) board))))

(defn coord-pairs [coords]
  (for [row coords
        col coords] [row col]))

(defn block-values [board coord]
  (let [third (fn [x] (* 3 (int (/ x 3))))
        corner (map third coord)
        block-coords (for [pair (coord-pairs [0 1 2])] (map + pair corner))]
    (set (map (fn [coord] (value-at board coord)) block-coords))))

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
  (set/difference 
    (set (range 1 10))
    (set/union
      (row-values board coord)
      (col-values board coord)
      (block-values board coord)))))

(defn filled? [board]
  (every? 
    (fn [coord] (has-value? board coord)) 
    (coord-pairs (range 9))))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every?
    (fn [values] (= values (set (range 1 10))))
    (rows board)))

(defn cols [board]
  (for [i (range 9)] 
    (set (map (fn [row] (get row i)) board))))

(defn valid-cols? [board]
  (every?
    (fn [values] (= values (set (range 1 10))))
    (cols board)))

(defn blocks [board]
  (for [y [0 3 6]
        x [0 3 6]]
    (block-values board [y x])))

(defn valid-blocks? [board]
  (every?
    (fn [values] (= values (set (range 1 10))))
    (blocks board)))

(defn valid-solution? [board]
  (and 
    (valid-rows? board)
    (valid-cols? board)
    (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (remove 
           (fn [coord] (has-value? board coord)) 
           (coord-pairs (range 9)))))

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board) [board] [])
    (let [coord (find-empty-point board)]
      (for [value (valid-values-for board coord)
            solution (solve-helper 
              (set-value-at board coord value))]
      solution))))

(defn solve [board]
  (first (solve-helper board)))
