(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[y _] coord]
    (set (map (fn [x] (value-at board [y x])) (range 9)))))

(defn col-values [board coord]
  (let [[_ x] coord]
    (set (map (fn [y] (value-at board [y x])) (range 9)))))

(defn coord-pairs [coords]
  (apply concat (map (fn [y] (map (fn [x] [y x]) coords)) coords)))

(defn block-values [board coord]
  (let [block-top-left (fn [[y x]] [(- y (mod y 3)) (- x (mod x 3))])
        [y x] (block-top-left coord)
        y-coords (apply concat (map (fn [y] (vector y y y)) (range y (+ y 3))))
        x-coords (apply concat (repeat 3 (range x (+ x 3))))]
    (set (map (fn [coord] (value-at board coord))
              (map vector y-coords x-coords)))))

(defn valid-values-for [board coord]
  (if (pos? (value-at board coord))
    #{}
    (clojure.set/difference
      all-values
      (row-values board coord)
      (col-values board coord)
      (block-values board coord))))

(defn filled? [board]
  (every? true? (for [row board] (every? pos? row))))

(defn rows [board]
  (map set board))

(defn valid-sets? [sets]
  (every? true? (map (fn [st] (= all-values st)) sets)))

(defn valid-rows? [board]
  (valid-sets? (rows board)))

(defn cols [board]
  (map (fn [col] (col-values board [0 col])) (range 9)))

(defn valid-cols? [board]
  (valid-sets? (cols board)))

(defn blocks [board]
  (map (fn [coord] (block-values board coord)) (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (valid-sets? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (some identity (for [y (range 9)
                       x (range 9)]
                   (if (has-value? board [y x])
                     false
                     [y x]))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      nil)
    (let [coord  (find-empty-point board)
          values (valid-values-for board coord)]
      (if (empty? values)
        nil
        (some identity (for [v values]
                            (solve (set-value-at board coord v))))))))

