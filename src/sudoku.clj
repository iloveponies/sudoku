(ns sudoku
  (:require [clojure.set :as set]))
(use 'clojure.tools.trace)

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (zero? (value-at board coord))
    false
    true)
  )

(defn row-values [board coord]
  (set (get board (first coord)))
  )

(defn col-values [board coord]
  ;; retry with for aye ??
  (let [col (last coord)]
    (loop [row 0
           a-set #{}]
      (if (= row 9)
        a-set
        (recur (inc row)
               (conj a-set (value-at board (vector row col)))))))
  )

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    (vector row col))
  )

(defn block-values [board coord]
  ;; For a given coordinates, get the set of values in the block this
  ;; coordinate is part of
  (let [x-coord (first coord)
        y-coord (last coord)
        top-left-x (* (quot x-coord 3) 3)
        top-left-y (* (quot y-coord 3) 3)
        block-pairs (for [row (range top-left-x (+ top-left-x 3))
                          col (range top-left-y (+ top-left-y 3))]
                      (vector row col))]
    (reduce #(conj %1 (value-at board %2)) #{} block-pairs))
  )

(defn valid-values-for [board coord]
  ( if (has-value? board coord)
    #{}
    (let [values-in-col (col-values board coord)
          values-in-row (row-values board coord)
          values-in-block (block-values board coord)
          all-used-values (set/union values-in-col values-in-row values-in-block)
          all-allowed-values (set (range 1 10))]
      (set/difference all-allowed-values all-used-values)))
  )

(defn filled? [board]
  (let [cur-values-in-board (reduce #(conj %1 (value-at board %2))
                                    []
                                    (coord-pairs (range 0 9)))]
    (if (some zero? cur-values-in-board)
      false
      true))
  )

(defn rows [board]
  (let [row-coords (for  [row-id (range 0 9)]
                     (vector row-id nil))]
    (reduce #(conj %1 (row-values board %2)) [] row-coords))
  )

(defn is-valid-* [a-set]
  ;; works for row/col/block
  ;; should find a better name
  (let [all-values (set (range 1 10))
        diff (set/difference all-values a-set)]
    (if (empty? diff)
      true
      false))
  )

(defn all-valid? [a-seq]
  (if (some false? (map is-valid-* a-seq))
    false
    true)
  )

(defn valid-rows? [board]
  (all-valid? (rows board))
  )

(defn cols [board]
  (let [col-coords (for [col-id (range 0 9)]
                     (vector nil col-id))]
    (reduce #(conj %1 (col-values board %2)) [] col-coords))
  )

(defn valid-cols? [board]
  (all-valid? (cols board))
  )

(defn blocks [board]
  (let [block-coords (coord-pairs [0 3 6])]
    (reduce #(conj %1 (block-values board %2)) [] block-coords))
  )

(defn valid-blocks? [board]
  (all-valid? (blocks board))
  )

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value)
  )

(defn next-coord [coord]
  ;; Given a co-ordinate on a sudoku board, returns the co-ordinate
  ;; next to it. Traversal is row first and if end of row is reached
  ;; then next row
  (let [[row col] coord]
    (if (= col 8)
      [(inc row) 0]
      [row (inc col)])
    )
  )

(defn find-empty-point [board]
  (loop [row 0
         col 0]
    (cond
     (and (= row 9) (= col 0)) nil
     ((complement  has-value?) board [row col]) [row col]
     :else (let [new-coords (next-coord [row col])]
             (recur (first new-coords) (last new-coords)))
     ))
  )

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      '())
    (for [empty-point [ (find-empty-point board)]
          possible-value  (valid-values-for board empty-point)
          solution (solve-helper (set-value-at board empty-point possible-value))]
      solution
      ))
  )


(defn solve [board]
  (first (solve-helper board))
  )
