(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board coord]
  (set (get board (get coord 0))))

(defn col-values [board coord]
  (set (map (fn [x] (get x (get coord 1))) board)))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-left-corner [coord]
  (loop [x (get coord 0)
         y (get coord 1)
         corner-values #{0 3 6}]
    (if (contains? corner-values x)
      (if (contains? corner-values y)
        [x y]
        (recur x (dec y) corner-values))
      (recur (dec x) y corner-values))))

(defn block-values [board coord]
  (loop [values []
         acc 1
         [left-corner-x left-corner-y] (block-left-corner coord)]
    (if (= acc 4)
      (set values)
      (recur (concat
              values
              (subvec (get board left-corner-x) left-corner-y (+ left-corner-y 3)))
             (inc acc) [(inc left-corner-x) left-corner-y]))))

(defn valid-values-for [board coord]
  (if (not (= 0 (get-in board coord)))
    #{}
    (set/difference all-values (set/union
                                (row-values board coord)
                                (col-values board coord)
                                (block-values board coord)))))

(defn filled-helper [board]
  (set (reduce (fn [res el] (concat res el)) [] board)))

(defn filled? [board]
  (not (contains? (filled-helper board) 0)))

(defn rows [board]
  (for [row (range 0 9)]
    (row-values board [row 0])))

(defn valid-rows? [board]
  (every? (fn [x] (= all-values x)) (rows board)))

(defn cols [board]
  (for [col (range 0 9)]
    (col-values board [0 col])))

(defn valid-cols? [board]
  (every? (fn [x] (= all-values x)) (cols board)))

(defn blocks [board]
  (for [row [0 3 6]
        col [0 3 6]]
    (block-values board [row col])))

(defn valid-blocks? [board]
  (every? (fn [x] (= all-values x)) (blocks board)))

(defn valid-solution? [board]
  (and (valid-blocks? board) (valid-rows? board) (valid-cols? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [x 0
         y 0]
    (cond
      (= 0 (get-in board [x y])) [x y]
      (= 9 y) (recur (inc x) 0)
      :else (recur x (inc y)))))


(defn solve [board]
  (cond
    (valid-solution? board) board
    (filled? board) []
    :else (let [empty-coord (find-empty-point board)]
            (for [x (valid-values-for board empty-coord)
                  solution (solve (set-value-at board empty-coord x))]
              solution))))
