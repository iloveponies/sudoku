(ns sudoku
  (:require [clojure.set :as set]))

(def all-values #{1 2 3 4 5 6 7 8 9})

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [row _]]
  (set (get board row)))

(defn col-values [board [_ col]]
  (set (map #(get % col) board)))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-values [board coord]
  (let [[r c] (map #(- % (mod % 3)) coord)]
    (set (for [x (range r (+ r 3))
               y (range c (+ c 3))]
           (value-at board [x y])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
    (set/difference all-values
                    (row-values   board coord)
                    (col-values   board coord)
                    (block-values board coord))))

(defn filled? [board]
  (not (some zero? (flatten board))))

(defn rows [board]
  (map #(row-values board [% 0]) (range 9)))

(defn valid-rows? [board]
  (every? #(= all-values %) (rows board)))

(defn cols [board]
  (map #(col-values board [0 %]) (range 9)))

(defn valid-cols? [board]
  (every? #(= all-values %) (cols board)))

(defn blocks [board]
  (map #(block-values board %) (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (every? #(= all-values %) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows?   board)
       (valid-cols?   board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (remove #(has-value? board %) (coord-pairs (range 9)))))

(defn solve [board]
  (if (valid-solution? board) board
    (let [coord (find-empty-point board)]
      (for [new-value (valid-values-for board coord)
            solution (solve (set-value-at board coord new-value))]
        solution))))
