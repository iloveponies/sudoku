(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (set (map #(get % (last coord)) board)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn round-coords [coord]
  (let [row (first coord)
        col (last coord)]
  [(* 3 (int (/ row 3))) (* 3 (int (/ col 3)))]))

(defn block-coords [coord]
  (for [row [(first coord) (+ 1 (first coord)) (+ 2 (first coord))]
        col [(last coord) (+ 1 (last coord)) (+ 2 (last coord))]]
    [row col]))

(defn block-values [board coord]
  (set (map #(value-at board %) (block-coords (round-coords coord)))))

(defn valid-values-for [board coord]
  (if 
    (zero? (value-at board coord))
    (let [rv (row-values board coord)
          cv (col-values board coord)
          bv (block-values board coord)]
      (set/difference (set/difference (set/difference all-values rv) cv) bv))
    #{}))

(defn filled? [board]
  (empty? (filter #(not (nil? %)) (map #(some #{0} %) board))))

(defn all-valid-values? [coll]
  (= all-values (set coll)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (and (filled? board)
       (empty? (filter #(not (all-valid-values? %)) (rows board)))))

(defn cols [board]
  (let [col-num (count (first board))
        cp (map vector (repeat col-num 0) (range col-num))]
    (map set (map #(col-values board %) cp))))

(defn valid-cols? [board]
  (and (filled? board)
       (empty? (filter #(not (all-valid-values? %)) (cols board)))))

(defn blocks [board]
  (let [block-coords (for [x [0 3 6]
                           y [0 3 6]]
                       [x y])]
    (map set (map #(block-values board %) block-coords))))

(defn valid-blocks? [board]
  (and (filled? board)
       (empty? (filter #(not (all-valid-values? %)) (blocks board)))))

(defn valid-solution? [board]
  (and
    (valid-cols? board)
    (valid-rows? board)
    (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn index-of [coll v]
  (loop [idx 0
         c coll]
    (cond
      (empty? c) nil
      (= v (first c)) idx
      :else (recur (inc idx) (rest c)))))

(defn find-empty-point [board]
  (loop [row 0
         da-board board]
    (cond
      (empty? da-board) nil
      (not (nil? (index-of (first da-board) 0))) [row (index-of (first da-board) 0)]
      :else(recur (inc row) (rest board)))))

(defn solve-helper [board]
  (if 
    (filled? board)
    (if 
      (valid-solution? board)
      [board]
      [])
    (let [empty-coord (find-empty-point board)
          valid-values (valid-values-for board empty-coord)]
      (for [vv valid-values
            solution (solve-helper (set-value-at board empty-coord vv))]
        solution)
    )))

(defn solve [board]
  (first (solve-helper board)))
