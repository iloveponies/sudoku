(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board [row _]]
  (set (get board row)))

(defn col-values [board [_ col]]
  (set (map (fn [row] (get row col)) board)))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn top-left [[row col]]
  [(* 3 (quot row 3))
   (* 3 (quot col 3))])

(defn block-values [board coord]
  (let [[top-row top-col] (top-left coord)]
    (set (for [row (range 3)
               col (range 3)]
            (value-at board [(+ top-row row) (+ top-col col)])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                    (set/union (row-values board coord)
                               (col-values board coord)
                               (block-values board coord)))))

(defn filled? [board]
  (every? (fn [coord] (has-value? board coord)) (coord-pairs (range 9))))

(defn valid-seq? [a-seq]
  (= all-values a-seq))

(defn rows [board]
  (for [row (range 9)]
    (row-values board [row row])))

(defn valid-rows? [board]
  (every? valid-seq? (rows board)))

(defn cols [board]
  (for [col (range 9)]
    (col-values board [col col])))

(defn valid-cols? [board]
  (every? valid-seq? (cols board)))

(defn blocks [board]
  (for [row (range 0 9 3)
        col (range 0 9 3)]
    (block-values board [row col])))

(defn valid-blocks? [board]
  (every? valid-seq? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn not-has-value? [board coord]
  (not (has-value? board coord)))

(defn find-empty-point [board]
  (first (filter (fn [coord] (not-has-value? board coord)) (coord-pairs (range 9)))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      '())
    (let [empty-point (find-empty-point board)]
      (for [value (valid-values-for board empty-point)
            solution (solve (set-value-at board empty-point value))]
      solution))))
