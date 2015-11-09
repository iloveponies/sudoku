(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord)))
)

(defn row-values [board [row col]]
  (set (get board row)))


(defn col-values [board [row col]]
  (reduce #(conj %1 (get %2 col)) #{} board)
)

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y])
)

(defn block-coords [[row col]]
  (let [ top  (- row (mod row 3)) bot   (+ top 3)
         left (- col (mod col 3)) right (+ left 3)]

  (for [ rows (range top bot)
         cols (range left right)]

    [rows cols])))

(defn block-values [board coord]
  (reduce #(conj %1 (value-at board %2)) #{} (block-coords coord))
)

(defn valid-values-for [board coord]

  (if (not= 0 (value-at board coord)) #{}
    (let [ used-values
           (set/union (row-values   board coord)
                      (col-values   board coord)
                      (block-values board coord))]

    (set/difference all-values used-values))))


(defn filled? [board]
  (empty? (flatten (map #(filter zero? %) board)))
)


(defn rows [board]
  (map #(row-values board [% 0]) (range 9))
)


(defn cols [board]
  (map #(col-values board [0 %]) (range 9))
)

(defn blocks [board]
  (let [ coords   [0 3 6]
         toplefts (for [r coords c coords] [r c]) ]

  (map #(block-values board %) toplefts)))

(defn valid-rows? [board]
  (empty? (filter #(not= all-values %) (rows board)))
)

(defn valid-cols? [board]
  (empty? (filter #(not= all-values %) (cols board)))
)

(defn valid-blocks? [board]
  (empty? (filter #(not= all-values %) (blocks board)))
)

(defn valid-solution? [board]
  (and (valid-blocks? board) (valid-rows? board) (valid-cols? board))
)

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value)
)

(defn empty-points [board]
  (for [  pairs        (coord-pairs (range 9))
          empty-points (filter #(zero? (value-at board %)) pairs)]
    empty-points ))

(defn find-empty-point [board]
  (first  (empty-points board)))

(defn solve [board]
  (if (valid-solution? board) board
    (if (find-empty-point board)
        (let [ next-coord   (find-empty-point board)
               next-values  (valid-values-for board next-coord)]
           (for [value next-values
                 solution (solve (set-value-at board next-coord value))]
                 solution )))))
