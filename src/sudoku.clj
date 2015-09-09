(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  ((complement zero?) (value-at board coord)))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord
        helper (fn [row] (get row col))]
    (set (map helper board))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board coord]
  (let [[row col] coord
        helper (fn [x] (* 3 (int (/ x 3))))
        [top left] [(helper row) (helper col)]]
    (set (for [x (range top (+ 3 top))
               y (range left (+ 3 left))]
           (value-at board [x y])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (set/union
                                 (row-values board coord)
                                 (col-values board coord)
                                 (block-values board coord)))))

(defn filled? [board]
  (reduce (fn [a b] (and a b)) (for [x (range 0 9)
                    y (range 0 9)]
                (has-value? board [x y]))))

(defn rows [board]
  (for [i (range 0 9)]
    (row-values board [i 0])))

(defn valid-kind? [board mapper]
  (reduce (fn [a b] (and a b)) (for [zone (mapper board)]
                                 (empty? (set/difference all-values zone)))))

(defn valid-rows? [board]
  (valid-kind? board rows))

(defn cols [board]
  (for [i (range 0 9)]
    (col-values board [0 i])))

(defn valid-cols? [board]
  (valid-kind? board cols))

(defn blocks [board]
  (for [i [0 3 6]
        j [0 3 6]]
    (block-values board [i j])))

(defn valid-blocks? [board]
  (valid-kind? board blocks))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [coords (apply vector (coord-pairs (range 0 9)))]
    (loop [idx 0]
      (cond
        (>= idx (count coords)) nil
        ((complement has-value?) board (get coords idx)) (get coords idx)
        :else (recur (inc idx)))
      )))



(defn solve-helper [board]
  (let [coord (find-empty-point board)]
    (if (nil? coord)
      (if (valid-solution? board)
        [board]
        '())
      (for [value (valid-values-for board coord)
            solution (solve-helper (set-value-at board coord value))]
        solution))))

(defn solve [board] (first (solve-helper board)))
