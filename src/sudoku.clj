(ns sudoku
  (:require [clojure.set :as set]))

(def all-values #{1 2 3 4 5 6 7 8 9})

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [row _]]
  (into #{} (get board row)))

(defn col-values [board [_ col]]
  (into #{} (map #(get % col) board)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-root [[row col]]
  [(* 3 (quot row 3)) (* 3 (quot col 3))])

(defn block-values [board coord]
  (let [[row col] (block-root coord)
        coords (map (fn [[r c]] [(+ r row) (+ c col)])
                    (coord-pairs [0 1 2]))]
    (into #{} (map #(value-at board %) coords))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                    (block-values board coord)
                    (row-values board coord)
                    (col-values board coord))))

(defn filled? [board]
  (not  (some zero? (flatten board))))

(defn rows [board]
  (map #(row-values board [% 0]) (range 9)))

(defn valid-rows? [board]
  (every? #(= all-values %) (rows board)))

(defn cols [board]
  (map #(col-values board [0 %]) (range 9)))

(defn valid-cols? [board]
  (every? #(= all-values %) (cols board)))

(defn blocks [board]
  (map #(into #{} %)
       (map #(block-values board %) (coord-pairs [0 3 6]))))

(defn valid-blocks? [board]
  (every? #(= all-values %) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (remove #(has-value? board %) (coord-pairs (range 9)))))

(defn solve [board]
  (if (valid-solution? board)
    board
    (let [p (find-empty-point board)]
      (for [v (valid-values-for board p)
            solution (solve (set-value-at board p v))]
        solution))))
