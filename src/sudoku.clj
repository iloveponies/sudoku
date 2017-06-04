(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= (value-at board coord) 0)))

(defn row-values [board [row _]]
  (set (get board row)))

(defn col-values [board [_ col]]
  (set
   (for [row board]
     (get row col))))

;; TODO: Not sure what to do with this
(defn coord-pairs [coords]
  (vec
   (for [row coords
         col coords]
     [row col])))

(defn row-col-pairs [rows cols]
  (for [row rows
        col cols]
    [row col]))

(defn get-top-left [[row col]]
  [(* (quot row 3) 3) (* (quot col 3) 3)])

(defn block-values [board coord]
  (let [[top-row top-col] (get-top-left coord)
        coords (row-col-pairs (range top-row (+ top-row 3))
                              (range top-col (+ top-col 3)))]
    (set
     (for [c coords]
       (value-at board c)))))

(defn filled-values [board coord]
  (reduce set/union ((juxt block-values col-values row-values) board coord)))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (filled-values board coord))))

(defn for-all-coords [board func]
  (for [row (range 0 9)
        col (range 0 9)]
    (func board [row col])))

(defn filled? [board]
  (every? true? (for-all-coords board has-value?)))

(defn rows [board]
  (for [row (range 0 9)]
    (row-values board [row 0])))

(defn is-valid? [board getter]
  (let [valid-thing? (fn [x] (= all-values x))]
    (empty? (filter (complement valid-thing?) (getter board)))))

(defn valid-rows? [board]
  (is-valid? board rows))

(defn cols [board]
  (for [col (range 0 9)]
    (col-values board [0 col])))

(defn valid-cols? [board]
  (is-valid? board cols))

(defn blocks [board]
  (for [row (range 0 9 3)
        col (range 0 9 3)]
    (block-values board [row col])))

(defn valid-blocks? [board]
  (is-valid? board blocks))

(defn valid-solution? [board]
  (every? true? ((juxt valid-rows? valid-blocks? valid-cols?) board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn empties [board]
  (for [row (range 0 9)
        col (range 0 9)
        :when ((complement has-value?) board [row col])]
    [row col]))

(defn find-empty-point [board]
  (first (empties board)))

(defn solve [board]
  nil)
