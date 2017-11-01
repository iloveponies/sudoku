(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (== 0 (value-at board coord))))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (set (map (fn [row] (get row col)) board))))

(defn coord-pairs [coords]
  (vec (for [row coords
             col coords]
    [row col])))

(defn block-coords [coord]
    (cond
      (<= coord 2) [0 1 2]
      (<= coord 5) [3 4 5]
      :else [6 7 8]))

(defn block-values [board coord]
  (let [[row col] coord]
    (set (for [x (block-coords row)
               y (block-coords col)]
          (value-at board [x y])))))
    
(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [used-values (set/union (row-values board coord)
                                 (col-values board coord)
                                 (block-values board coord))]
      (set/difference all-values used-values))))

(defn filled? [board]
  (let [all-coords (coord-pairs (range 9))
        board-values (set (map (fn [coord] (value-at board coord)) all-coords))]
    (not (contains? board-values 0))))

(defn rows [board]
  (map (fn [row] (row-values board [row 0])) (range 9)))

(defn cols [board]
  (map (fn [col] (col-values board [0 col])) (range 9)))

(defn blocks [board]
  (map (fn [coord] (block-values board coord)) (coord-pairs [0 3 6])))

(defn valid-sets? [sets]
  (every? (fn [set] (= all-values set)) sets))

(defn valid-rows? [board]
  (valid-sets? (rows board)))

(defn valid-cols? [board]
  (valid-sets? (cols board)))

(defn valid-blocks? [board]
  (valid-sets? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [all-coords (coord-pairs (range 9))]
    (let [coord (first all-coords)]
      (if (not (has-value? board coord))
        coord
        (recur (rest all-coords))))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      ())
    (let [empty-point (find-empty-point board)]
      (for [possible-value (valid-values-for board empty-point)
            solution (solve (set-value-at board empty-point possible-value))]
        solution))))
