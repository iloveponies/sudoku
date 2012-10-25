(ns sudoku.core
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (set (nth board (first coord))))

(defn col-values [board coord]
  (set (map #(nth %1 (second coord)) board)))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-values [board [x y]]
  (let [top-left-coord (fn [x y] [(* 3 (quot x 3))
                                  (* 3 (quot y 3))])
        vec-sum (fn [x y] [(+ (first x) (first y))
                           (+ (second x) (second y))])]
    (set (map #(value-at board %)
              (map (partial vec-sum (top-left-coord x y)) (coord-pairs [0 1 2]))))))

(defn valid-values-for [board coord]
  (set/difference all-values (set/union (block-values board coord) (row-values board coord) (col-values board coord))))

(defn filled? [board]
  (not (some zero? (flatten board))))

(defn rows [board]
  (map #(row-values board [% 0]) (range 0 9)))

(defn valid-rows? [board]
  (every? #(set/subset? all-values %) (rows board)))

(defn cols [board]
  (map #(col-values board [0 %]) (range 0 9)))

(defn valid-cols? [board]
  (every? #(set/subset? all-values %) (cols board)))

(defn blocks [board]
  (map #(block-values board %) (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (every? #(set/subset? all-values %) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter #(zero? (value-at board %)) (coord-pairs (range 0 9)))))

(defn solve-brute-force [board]
  (if (filled? board)
    board
    (let [point (find-empty-point board)]
      (flatten (map #(solve-brute-force (set-value-at board point %)) (valid-values-for board point))))))
    
(defn solve [board]
  (partition 9 (solve-brute-force board)))