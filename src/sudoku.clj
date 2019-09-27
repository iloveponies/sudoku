(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board [row _]]
  (set (get board row)))

(defn col-values [board [_ col]]
  (reduce (fn [acc row] (conj acc (get row col))) #{} board))

(defn coord-pairs [coords]
  (for [row coords
        coll coords]
    [row coll]))

(defn block-values [board [row col]]
  (let [top-left (fn [row-or-col] (- row-or-col (mod row-or-col 3)))
        block-rows (range (top-left row) (+ (top-left row) 3))
        block-cols (range (top-left col) (+ (top-left col) 3))
        block-coords (for [a-row block-rows
                           a-col block-cols]
                       [a-row a-col])]
    (reduce (fn [acc coord] (conj acc (value-at board coord))) #{} block-coords)))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                    (set/union (row-values board coord)
                               (col-values board coord)
                               (block-values board coord)))))

(defn row-coords [board]
  (range 0 (count board)))

(defn col-coords [board]
  (row-coords board))

(defn filled? [board]
  (let [board-numbers (fn [board]
                        (map (fn [coord] (value-at board coord))
                             (coord-pairs (row-coords board))))]
    (not (some #(= 0 %) (board-numbers board)))))

(defn rows [board]
  (map (fn [row-coord] (row-values board [row-coord 0])) (row-coords board)))

(defn valid? [values]
  (empty? (set/difference all-values values)))

(defn valid-rows? [board]
  (every? valid? (rows board)))

(defn cols [board]
  (map (fn [col-coord] (col-values board [0 col-coord])) (col-coords board)))

(defn valid-cols? [board]
  (every? valid? (cols board)))

(defn blocks [board]
  (let [top-left (fn [[row col]] [(- row (mod row 3)) (- col (mod col 3))])
        block-coords (fn [board] (remove nil?
                                         (for [row (row-coords board)
                                               col (col-coords board)]
                                           (if (and (= 0 (mod row 3))
                                                    (= 0 (mod col 3)))
                                             [row col]
                                             nil))))]
    (map (fn [block-coord] (block-values board block-coord)) (block-coords board))))

(defn valid-blocks? [board]
  (every? valid? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [board-coords (fn [board] (coord-pairs (range 0 (count board))))]
    (some (fn [coord] (if (not (has-value? board coord)) coord nil)) (board-coords board))))

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board) [board] [])
    (let [empty-point (find-empty-point board)
          valid-point-values (valid-values-for board empty-point)]
      (for [value valid-point-values
            solution (solve-helper (set-value-at board empty-point value))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))
