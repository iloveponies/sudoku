(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn valid-set? [a-set]
  (empty? (set/difference all-values a-set)))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (pos? (value-at board coord)))

(defn row-values [board coord]
  (let [[y x] coord]
    (set (get board y))))

(defn col-values [board coord]
  (let [[y x] coord]
    (set (map #(get % x) board))))

(defn coord-pairs [coords]
  (for [a coords
        b coords]
    [a b]))

(defn block-values [board coord]
  (let [[y x] coord
        by (* 3 (quot y 3))
        bx (* 3 (quot x 3))
        f (fn [[y x]]
            (value-at board [(+ y by) (+ x bx)]))]
    (set (map f (coord-pairs (range 3))))))

(defn valid-values-for [board coord]
  (if (contains? all-values (value-at board coord))
    #{}
    (set/difference all-values
                    (row-values board coord)
                    (col-values board coord)
                    (block-values board coord))))

(defn filled? [board]
  (every? pos? (flatten board)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? valid-set? (rows board)))

(defn cols [board]
  (map set (partition 9 (apply interleave board))))

(defn valid-cols? [board]
  (every? valid-set? (cols board)))

(defn blocks [board]
  (let [cols (partition 9 (apply interleave board))
        blocks (map flatten (partition 3 (apply interleave (map (partial partition 3) cols))))]
    (map set blocks)))

(defn valid-blocks? [board]
  (every? valid-set? (blocks board)))

(defn valid-solution? [board]
  (and valid-cols? board) (and (valid-rows? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (complement (partial has-value? board)) (coord-pairs (range 9)))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      [])
    (let [empty-point (find-empty-point board)
          valid-values (valid-values-for board empty-point)]
      (for [value valid-values
            solution (solve (set-value-at board empty-point value))]
        solution)
      )))
