(ns sudoku
  (:require [clojure.set :as set]))

(def all-values #{1 2 3 4 5 6 7 8 9})

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (== (value-at board coord) 0)))

(defn row-values [board coord]
  (let [[row col] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[row col] coord
        m-get (fn [x] (get x col))]
    (set (map m-get board))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board coord]
  (let [[raw-row raw-col] coord
        row (* (int (/ raw-row 3)) 3)
        col (* (int (/ raw-col 3)) 3)]
    (set (for [y [row (+ row 1) (+ row 2)]
               x [col (+ col 1) (+ col 2)]]
           (value-at board [y x])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (reduce set/difference all-values [(block-values board coord)
                                       (row-values board coord)
                                       (col-values board coord)])))

(defn filled? [board]
  (not (some
   zero?
   (reduce concat board))))

(defn rows [board]
  (for [row (range 0 9)]
    (row-values board [row 0])))

(defn all-valid? [a-seq]
  (let [diff   (partial set/difference all-values)
        valid? (fn [x] (empty? (diff x)))]
    (every? valid? a-seq)))

(defn valid-rows? [board]
  (all-valid? (rows board)))

(defn cols [board]
  (for [col (range 0 9)]
    (col-values board [0 col])))

(defn valid-cols? [board]
  (all-valid? (cols board)))

(defn blocks [board]
  (for [row (range 0 9 3)
        col (range 0 9 3)]
    (block-values board [row col])))

(defn valid-blocks? [board]
  (all-valid? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [row (fn [i] (int (/ i 9)))
        col (fn [i] (mod i 9))
        coord (fn [i] [(row i) (col i)])]
  (loop [i 0]
    (cond
     (>= i 81)
      nil
     (not (has-value? board (coord i)))
      (coord i)
     :else
      (recur (inc i))))))

(defn solve-helper [board]
  (let [empty-loc (find-empty-point board)]
    (if (filled? board)
      (if (valid-solution? board)
        [board]
        '())
      (for [valid-value (valid-values-for board empty-loc)
            solution (solve-helper
                      (set-value-at board empty-loc valid-value))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))
