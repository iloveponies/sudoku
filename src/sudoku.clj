(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (let [[x y] coord]
  (get-in board [x y])))

(defn has-value? [board coord]
    (not= (value-at board coord) 0))

(defn row-values [board coord]
  (let [[x y] coord]
    (set (get board x))))

(defn col-values [board coord]
  (let [[x y] coord]
    (set (map (fn [row] (get row y)) board))))

(defn coord-pairs [coords]
  (for [row coords
       col coords]
    [row col]))

(defn block-origin [row col]
  [(* 3 (int (/ row 3))) (* 3 (int (/ col 3)))])

(defn block-values [board [row col]]
  (set (let [[cr cc] (block-origin row col)]
      (map
       (fn [[x y]] (value-at board [(+ x cr) (+ y cc)]))
       (coord-pairs [0 1 2])))))

(defn valid-values-for [board coord]
  (cond
   (zero? (value-at board coord)) (set/difference all-values
                                                  (set/union
                                                   (row-values board coord)
                                                   (block-values board coord)
                                                   (col-values board coord)))
   :else #{}))

(defn filled? [board]
  (not (contains? (set (map (fn [x] (value-at board x))
     (coord-pairs (range 0 9)))) 0)))

(defn rows [board]
  (for [row (range 0 9)]
    (row-values board [row 0])))

(defn valid-rows? [board]
  (every? (fn [x] (= x all-values)) (rows board)))

(defn cols [board]
  (for [col (range 0 9)]
    (col-values board [0 col])))

(defn valid-cols? [board]
  (every? (fn [x] (= x all-values)) (cols board)))

(defn blocks [board]
  (for [row [0 3 6]
        col [0 3 6]]
     (block-values board [row col])))

(defn valid-blocks? [board]
  (every? (fn [x] (= x all-values)) (blocks board)))

(defn valid-solution? [board]
  (cond
   (not (filled? board)) false
   (not (valid-rows? board)) false
   (not (valid-cols? board)) false
   (not (valid-blocks? board)) false
   :else true))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter #(= 0 (value-at board %)) (coord-pairs (range 10)))))

;Recap of backtracking:
;check if you are at the end
;if so, is the solution valid?
;if not, return an empty sequence
;otherwise return [solution]
;if not
;select an empty location
;try solving with each valid value for that location


(defn solver [board]
  (if(filled? board)
    (if(valid-solution? board) [board]
      [])
    (let [emptyLocation (find-empty-point board)]
      (for [valid-value (valid-values-for board emptyLocation)
            solution (solver (set-value-at board emptyLocation valid-value))]
        solution))))

(defn solve [board]
  (first (solver board)))
