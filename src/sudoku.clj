(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [row col]]
  (set (nth board row)))

(defn col-values [board [row col]]
  (set (nth (apply map vector board) col)))


(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn reduce-coord [x]
  (cond
   (< x 3) 0
   (< x 6) 3
   :else 6))

(defn top-left-of-block [[row col]]
  [(reduce-coord row) (reduce-coord col)])

(defn block-values [board coord]
  (let [[top-row top-col] (top-left-of-block coord)]
    (set (for [row (range top-row (+ top-row 3))
          col (range top-col (+ top-col 3))]
           (value-at board [row col])
           ))))


(defn valid-values-for [board coord]
  (if (= 0 (value-at board coord))
    (set/difference all-values
                    (set/union
                     (block-values board coord)
                     (row-values board coord)
                     (col-values board coord)))
    #{}))


(defn all-numbers-on-board [board]
  (set (for [coord (coord-pairs (range 0 9))]
    (value-at board coord))))

(defn filled? [board]
  (not (contains? (all-numbers-on-board board) 0)))

(defn all-true? [coll]
  (not (some false? coll)))

(defn valid? [coll]
  (and (= (count coll) 9)
       (all-true?
              (for [numba (range 1 10)]
                 (contains? coll numba)))))

(defn rows [board]
   (for [row (range 0 9)]
    (row-values board [row 0])))

(defn valid-rows? [board]
  (all-true? (for [row (rows board)]
               (valid? row))))

(defn cols [board]
  (for [col (range 0 9)]
    (col-values board [0 col])))

(defn valid-cols? [board]
    (all-true? (for [col (cols board)]
               (valid? col))))

(defn blocks [board]
  (for [row (range 0 9 3)
        col (range 0 9 3)]
    (block-values board [row col])))

(defn valid-blocks? [board]
    (all-true? (for [block (blocks board)]
               (valid? block))))

(defn valid-solution? [board]
  (and
   (filled? board)
   (valid-rows? board)
   (valid-cols? board)
   (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
    (loop [coords (coord-pairs (range 0 9))]
      (cond
       (empty? coords) nil
       (= 0 (value-at board (first coords))) (first coords)
       :else (recur (rest coords)))))

(defn solve [board]
  nil)
