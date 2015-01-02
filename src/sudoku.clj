(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [row _]]
  (set (get board row)))

(defn col-values [board [_ col]]
  (set (map (fn [row] (get row col)) board)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board coord]
  (let [top-left (map (fn [x] (* 3 (int (/ x 3)))) coord)]
    (set (map
           (fn [x] (value-at board (map + top-left x)))
           (coord-pairs [0 1 2])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (set/union (row-values board coord)
                                          (col-values board coord)
                                          (block-values board coord)))))

(defn filled? [board]
  (every? boolean (map (fn [coord] (has-value? board coord))
                       (coord-pairs (range 9)))))

(defn valid-sec? [f board]
  (every? (fn [s] (empty? (set/difference all-values s)))
          (f board)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (valid-sec? rows board))

(defn cols [board]
  (map (fn [col] (col-values board [0 col])) (range 9)))

(defn valid-cols? [board]
  (valid-sec? cols board))

(defn blocks [board]
  (let [row (fn [x] (* 3 (int (/ x 3))))
        col (fn [x] (* 3 (mod x 3)))]
    (map (fn [i] (block-values board [(row i) (col i)])) (range 9))))

(defn valid-blocks? [board]
  (valid-sec? blocks board))

(defn valid-solution? [board]
  (and
   (valid-rows? board)
   (valid-cols? board)
   (valid-blocks? board)
   ))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (fn [coord] (not (has-value? board coord)))
                 (coord-pairs (range 9)))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board) board [])
    (let [free  (find-empty-point board)
          valid (valid-values-for board free)]
      (first (filter (complement empty?)
                     (map (fn [v] (solve (set-value-at board free v)))
                          valid))))))
