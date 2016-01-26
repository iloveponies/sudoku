(ns sudoku
  (:require [clojure.set :as set]))

(def all-values #{1 2 3 4 5 6 7 8 9})

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= (value-at board coord) 0))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (loop [b board
         result #{}]
    (if (empty? b)
      result
      (recur (rest b) (conj result (get (first b) (second coord)))))))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-values [board coord]
  (let [top-left (fn [[x y]]
                   [(* (int (/ x 3)) 3) (* (int (/ y 3)) 3)])
        block-seq (map (fn [[x y]]
                         (value-at board [(+ (first (top-left coord)) x)
                                          (+ (second (top-left coord)) y)]))
                       (coord-pairs [0 1 2]))]
    (set block-seq)))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (block-values board coord) (row-values board coord) (col-values board coord))))

(defn filled? [board]
  (every? (fn [c] (has-value? board c)) (coord-pairs (range 0 9))))

(defn rows [board]
  (for [y (range 0 9)]
    (row-values board [y 0])))

(defn valid-rows? [board]
  (every? (fn [row] (empty? (set/difference all-values row))) (rows board)))

(defn cols [board]
  (for [x (range 0 9)]
    (col-values board [0 x])))

(defn valid-cols? [board]
  (every? (fn [col] (empty? (set/difference all-values col))) (cols board)))

(defn blocks [board]
  (for [y [0 3 6]
        x [0 3 6]]
    (block-values board [y x])))

(defn valid-blocks? [board]
  (every? (fn [block] (empty? (set/difference all-values block))) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (some (fn [xy]
          (if (not (has-value? board xy))
            xy
            false))
        (coord-pairs (range 0 9))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      [])
    (let [empty-point (find-empty-point board)
          remaining (valid-values-for board empty-point)]
      (for [try-val remaining
            solution (solve (set-value-at board empty-point try-val))]
        solution))))
