(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (set (get board (get coord 0))))

(defn col-values [board coord]
  (let [[x y] coord]
    (set (map (fn [row] (get row y)) board))))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    (vector x y)))

(defn top-left-corner [coord]
  (let [[x y] coord]
    (vector (* (quot x 3) 3) (* (quot y 3) 3))))

(def first-square-coords (coord-pairs [0 1 2]))

(defn block-values [board coord]
  (let [top-left (top-left-corner coord)
        top-left-x (get top-left 0)
        top-left-y (get top-left 1)
        add-x (fn [c] (vector (+ (get c 0) top-left-x) (get c 1)))
        add-y (fn [c] (vector (get c 0) (+ (get c 1) top-left-y)))
        coords (map add-y(map add-x first-square-coords))]
    (set (map (fn [y] (value-at board y)) coords))))

(defn valid-values-for [board coord]
  (if (not (zero? (value-at board coord))) #{}
    (let [invalid (set/union (row-values board coord) (set/union (col-values board coord) (block-values board coord)))]
      (set/difference all-values invalid))))

(defn filled? [board]
  (let [numbers (set (reduce concat board))]
    (not (contains? numbers 0))))

(defn rows [board]
  (loop [i 0
         result []
         brd board]
    (if (= i 9) result
      (recur (inc i) (conj result (row-values brd [i 0])) brd))))

(defn valid-rows? [board]
  (let [valid #{1 2 3 4 5 6 7 8 9}]
    (apply = (concat (rows board) (vector valid)))))

(defn cols [board]
  (loop [i 0
         result []
         brd board]
    (if (= i 9) result
      (recur (inc i) (conj result (col-values brd [0 i])) brd))))

(defn valid-cols? [board]
  (let [valid #{1 2 3 4 5 6 7 8 9}]
    (apply = (concat (cols board) (vector valid)))))

(defn blocks [board]
  (let [test-coords [[0 0] [0 3] [0 6] [3 0] [3 3] [3 6] [6 0] [6 3] [6 6]]]
    (reduce conj [] (map (fn [d] (block-values board d)) test-coords))))

(defn valid-blocks? [board]
  (let [valid #{1 2 3 4 5 6 7 8 9}]
    (apply = (concat (blocks board) (vector valid)))))

(defn valid-solution? [board]
  (and (valid-rows? board) (and (valid-cols? board) (valid-blocks? board))))

(defn set-value-at [board coord new-value]
  (let [[x y] coord]
    (assoc board x (assoc (get board x) y new-value))))

(defn find-empty-point [board]
  (let [coords (mapv identity (coord-pairs [0 1 2 3 4 5 6 7 8]))
        n (dec (count coords))]
    (loop [i n]
      (let [coord (get coords i)]
        (if (zero? (value-at board coord)) coord
          (recur (dec i)))))))

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board) [board]
    '())
    (let [coord (find-empty-point board)
          valid-values (valid-values-for board coord)]
      (for [number valid-values
          solution (solve-helper (set-value-at board coord number))]
      solution))))

(defn solve [board]
  (let [solution (solve-helper board)]
    (if (seq? solution) (first solution)
      solution)))
