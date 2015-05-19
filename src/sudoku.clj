(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (loop [x 0
         values #{}]
    (if (= x 9)
      values
      (recur
       (inc x)
       (conj values (get-in board [x (second coord)]))))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    (vector row col)))

(defn corner-value [coord]
  (let [round (fn [x]
                  (cond
                   (<= 0 x 2) 0
                   (<= 3 x 5) 3
                   :else
                   6))
        [y x] coord]
   (vector (range (round y) (+ 3 (round y)))
           (range (round x) (+ 3 (round x))))))

(defn block-values [board coord]
 (let [[col row] (corner-value coord)
       all-coords (for [y col
                        x row]
                    (vector y x))]
  (set (for [coords all-coords]
    (value-at board coords)))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference
     all-values
     (set/union (block-values board coord)
                (row-values board coord)
                (col-values board coord)))))

(defn filled? [board]
 (let [every-value
       (for [coord (coord-pairs (range 0 9))]
                   (value-at board coord))]
   (not (contains? (set every-value) 0))))

(defn rows [board]
  (for [row (range 0 9)]
    (row-values board [row 2])))

(defn valid-rows? [board]
  (let [valid (for [row (rows board)]
                (= row all-values))]
    (not (contains? (set valid) false))))

(defn cols [board]
  (for [col (range 0 9)]
    (col-values board [2 col])))

(defn valid-cols? [board]
  (let [valid (for [col (cols board)]
                (= col all-values))]
    (not (contains? (set valid) false))))

(defn blocks [board]
  (for [block [[0 0] [0 3] [0 6]
               [3 0] [3 3] [3 6]
               [6 0] [6 3] [6 6]]]
    (block-values board block)))

(defn valid-blocks? [board]
  (let [valid (for [block (blocks board)]
                (= block all-values))]
    (not (contains? (set valid) false))))

(defn valid-solution? [board]
  (and (valid-blocks? board)
       (valid-cols? board)
       (valid-rows? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [a-seq (coord-pairs (range 0 9))]
    (cond
     (empty? a-seq)
      nil
     (has-value? board (first a-seq))
      (recur (rest a-seq))
     :else
      (first a-seq))))

(defn solve [board]
  (if (filled? board)
   (if (valid-solution? board)
    board
    ())
    (let [coord (find-empty-point board)
          values (valid-values-for board coord)]
      (for [value values
            solution (solve (set-value-at board coord value))]
        solution))))
