(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (zero? (value-at board coord))
    false
    true))

(defn row-values [board coord]
  (set (board (first coord))))

(defn col-values [board coord]
  (reduce #(conj %1 (%2 (last coord))) #{} board))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    (vector row col)))

(defn block-top-left-pos [pos]
  (cond
    (<= pos 2) 0
    (<= pos 5) 3
    :else 6))

(defn block-top-left-coord [coord]
  (let [row (first coord)
        col (last coord)]
    (vector (block-top-left-pos row) (block-top-left-pos col))))

(defn block-values [board coord]
  (let [top-left (block-top-left-coord coord)
        start-row (first top-left)
        start-col (last top-left)
        rows (range start-row (+ start-row 3))
        cols (range start-col (+ start-col 3))
        coords (for [rb rows
                     cb cols]
                 (vector rb cb))]
    (->> coords
         (map #(value-at board %))
         set)))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (set/union (row-values board coord)
                                          (col-values board coord)
                                          (block-values board coord)))))

(defn filled? [board]
  (not-any? #(contains? (set %) 0) board))

(defn rows [board]
  (map-indexed (fn [idx _] (row-values board [idx idx])) board))

(defn valid-rows? [board]
  (every? #(= all-values %) (rows board)))

(defn cols [board]
  (map-indexed (fn [idx _] (col-values board [idx idx])) board))

(defn valid-cols? [board]
  (every? #(= all-values %) (cols board)))

(defn blocks [board]
  (for [row (range 3)
        col (range 3)]
    (set (block-values board [(* row 3) (* col 3)]))))

(defn valid-blocks? [board]
  (every? #(= all-values %) (blocks board)))


(defn valid-solution? [board]
  (and
    (valid-rows? board)
    (valid-cols? board)
    (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (->> (for [row (range (count all-values))
        col (range (count all-values))]
    [row col])
       (filter #(zero? (value-at board %)))
       first))

(defn solve [board]
  (letfn [(guess-next-move-board [coord value]
            (set-value-at board coord value))
          (guess-next-move-boards []
            (let [coord (find-empty-point board)]
              (map #(solve (guess-next-move-board coord %)) (valid-values-for board coord))))]
    (cond
      (valid-solution? board) board
      (filled? board) nil
      :else (some identity (guess-next-move-boards)))))
