(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (all-values (value-at board coord)))

(def board-size 9)
(def block-size 3)

(defn row-values [board [y _]]
  (set (map #(value-at board [y %]) (range board-size))))

(defn col-values [board [_ x]]
  (set (map #(value-at board [% x]) (range board-size))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board [row col]]
  (let [block-top-left-corner [(* block-size (int (/ row block-size)))
                               (* block-size (int (/ col block-size)))]]
    (set (for [y (range block-size) x (range block-size)]
           (value-at board (map + [y x] block-top-left-corner))))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                    (col-values board coord)
                    (row-values board coord)
                    (block-values board coord))))

(defn filled? [board]
  (every? #(has-value? board %)
          (coord-pairs (range board-size))))

(defn rows [board]
  (for [row (range board-size)]
    (row-values board [row 0])))

(defn valid-rows? [board]
  (every? #(= all-values %) (rows board)))

(defn cols [board]
  (for [col (range board-size)]
    (col-values board [0 col])))

(defn valid-cols? [board]
  (every? #(= all-values %) (cols board)))

(defn blocks [board]
  (for [block-y (range (/ board-size block-size))
        block-x (range (/ board-size block-size))]
    (block-values board
                  [(* block-y block-size) (* block-x block-size)])))

(defn valid-blocks? [board]
  (every? #(= all-values %) (blocks board)))

(defn valid-solution? [board]
  (every? #(% board) [valid-rows? valid-cols? valid-blocks?]))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (remove #(has-value? board %)
                 (coord-pairs (range board-size)))))

(defn solve-helper [board]
  (let [empty-point-coord (find-empty-point board)]
    (if (nil? empty-point-coord)
      (if (valid-solution? board)
        [board]
        [])
      (for [value (valid-values-for board empty-point-coord)
            solution (solve-helper
                      (set-value-at board empty-point-coord value))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))
