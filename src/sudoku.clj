(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [row _]]
  (set (nth board row)))

(defn col-values [board [_ col]]
  (set (map #(value-at board [% col]) (range 9))))

(defn coord-pairs [coords]
  (for [c1 coords
        c2 coords]
    [c1 c2]))

(defn block-coords [board coord]
  (let [block (map #(quot % 3) coord)
        left-up-block (coord-pairs (range 3))]
    (map (fn [c] (map #(+ (* 3 %1) %2) block c)) left-up-block)
    ))

(defn block-values [board coord]
   (set (map #(value-at board %) (block-coords board coord))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                    (row-values board coord)
                    (col-values board coord)
                    (block-values board coord))))

(defn filled? [board]
  (let [all-coords (coord-pairs (range 9))]
    (every? (partial has-value? board) all-coords)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? #(= all-values %) (rows board)))

(defn cols [board]
  (map #(col-values board [0 %]) (range 9)))

(defn valid-cols? [board]
  (every? #(= all-values %) (cols board)))

(defn blocks [board]
  (let [left-up-block (coord-pairs (range 3))
        all-block-corners (map (fn [[x y]] [(* 3 x) (* 3 y)]) left-up-block)]
    (map (partial block-values board) all-block-corners)))

(defn valid-blocks? [board]
    (every? #(= all-values %) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn index-of [item coll]
  (count (take-while (partial not= item) coll)))

(defn find-empty-point [board]
  (let [empty-points (filter (complement (partial has-value? board)) (coord-pairs (range 9)))
        vals (map (partial valid-values-for board) empty-points)
        val-count (map count vals)
        min-index (index-of (apply min val-count) val-count)]
    (nth empty-points min-index)
    ))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      [])
    (let [x (find-empty-point board)
          vals (valid-values-for board x)
          sols (map #(solve (set-value-at board x %)) vals)]
        (apply concat sols))))

















