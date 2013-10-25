(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= (value-at board coord) 0))

(defn row-values [board [r c]]
  (set (get board r)))

(defn col-values [board [r c]]
  (set (map #(get % c) board)))

(defn coord-pairs [coords]
  (for [x coords, y coords]
    [x y]))

(defn- block-corner [r c]
  [(* 3 (int (/ r 3))) (* 3 (int (/ c 3)))])

(defn block-values [board [r c]]
  (set (let [[cr cc] (block-corner r c)]
    (map (fn [[x y]] (value-at board [(+ x cr) (+ y cc)])) (coord-pairs [0 1 2])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
    (set/difference all-values (row-values board coord) (col-values board coord) (block-values board coord))))

(defn filled? [board]
  (every? #(every? (fn [x] (> x 0)) %) board))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (and (filled? board) (every? #(= all-values %) (rows board))))

(defn- transpose [board]
  (apply map vector board))

(defn cols [board]
  (map set (transpose board)))

(defn valid-cols? [board]
  (and (filled? board) (every? #(= all-values %) (rows board))))

(defn blocks [board]
  (for [i [0 1 2]
        j [0 1 2]]
    (block-values board [(* 3 i) (* 3 j)])))

(defn valid-blocks? [board]
  (and (filled? board) (every? #(= all-values %) (blocks board))))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter #(= 0 (value-at board %)) (coord-pairs (range 10)))))

(defn- solve-helper [board]
  (cond
    (filled? board) (if (valid-solution? board) [board] [])
    :else (let [empty (find-empty-point board)]
            (apply concat (map #(solve-helper (set-value-at board empty %)) (valid-values-for board empty))))))

(defn solve [board]
  (first (solve-helper board)))
