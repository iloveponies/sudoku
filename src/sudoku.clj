(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)



(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (== (value-at board coord) 0)
    false
    true))

(defn row-values [board coord]
  (let [[col _] coord]
    (set (map (fn [row]
                (get-in board [col row])) (range 9)))))

(defn col-values [board coord]
  (let [[_ row] coord]
    (set (map (fn [col]
           (get-in board [col row])) (range 9)))))

(defn coord-pairs [coords]
  (for [col coords
        row coords]
    [col row]))

(defn block-values [board coord]
  (let [upper-right (fn [[x y]]
                      (cond
                        (< x 3)
                          (cond
                            (< y 3)
                              [0 0]
                            (< y 6)
                              [0 3]
                            :else
                              [0 6])
                        (< x 6)
                          (cond
                            (< y 3)
                              [3 0]
                            (< y 6)
                              [3 3]
                            :else
                              [3 6])
                        :else
                          (cond
                            (< y 3)
                              [6 0]
                            (< y 6)
                              [6 3]
                            :else
                              [6 6])))]
    (let [rangex (map (fn [number] (+ number (first (upper-right coord)))) [0 1 2])
          rangey (map (fn [number] (+ number (second (upper-right coord)))) [0 1 2])]
      (set (for [x rangex
            y rangey]
        (value-at board [x y]))))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (set/union (block-values board coord) (row-values board coord) (col-values board coord)))))

(defn filled? [board]
    (boolean (not (some true? (for [x (range 9)]
      (contains? (row-values board [x 0]) 0))))))

(defn rows [board]
  (for [x (range 9)]
    (row-values board [x 0])))

(defn valid-rows? [board]
  (every? true? (for [x (range 9)]
    (and (not (contains? (row-values board [x 0]) 0)) (== 9 (count (row-values board [x 0])))))))

(defn cols [board]
  (for [x (range 9)]
    (col-values board [0 x])))

(defn valid-cols? [board]
  (every? true? (for [x (range 9)]
    (and (not (contains? (col-values board [0 x]) 0)) (== 9 (count (col-values board [0 x])))))))

(defn blocks [board]
  (for [x [0 3 6]
        y [0 3 6]]
    (block-values board [x y])))

(defn valid-blocks? [board]
  (every? true? (for [x [0 3 6]
                      y [0 3 6]]
    (and (not (contains? (block-values board [x y]) 0)) (== 9 (count (block-values board [x y])))))))

(defn valid-solution? [board]
  (and (valid-blocks? board) (valid-rows? board) (valid-cols? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (remove nil? (set (for [x (range 9)
        y (range 9)]
    (if (has-value? board [x y])
      nil
      [x y]))))))

(defn solve [board]
    (if (valid-solution? board)
      board
      (let [empty-point (find-empty-point board)]
        (for [value (valid-values-for board empty-point)
          solved (solve (set-value-at board empty-point value))]
          solved))))

