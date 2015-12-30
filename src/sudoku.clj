(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board [row col]]
  (set (get board row)))

(defn col-values [board [row col]]
  (loop [r 0
         values #{}]
    (if (== r 9)
      values
      (recur
       (inc r)
       (conj values (value-at board [r col]))))))

(defn coord-pairs [coord-sequence]
  (for [r coord-sequence
        c coord-sequence]
    [r c]))

(defn block-values [board [x y]]
  (def shifted-coords
    (fn [[shif-top shift-left]]
      (map (fn [[row col]] [(+ row shif-top) (+ col shift-left)])
           (coord-pairs [3 4 5]))))

  (let [[top left] [(* (quot x 3) 3) (* (quot y 3) 3)]
        shifts [(- top 3) (- left 3)]
        real-coords (shifted-coords shifts)]
    (set
     (map
      (fn [coord] (value-at board coord))
      real-coords))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference
     all-values
     (row-values board coord)
     (col-values board coord)
     (block-values board coord))))

(defn filled? [board]
  (every? true?
   (for [row (range 9)
         col (range 9)]
     (has-value? board [row col]))))

(defn rows [board]
  (map
   (fn [row] (row-values board [row 0]))
   (range 9)))

(defn valid? [a-seq]
  (every?
   (fn [sub-seq]
     (and
      (== (count sub-seq) 9)
      (not (contains? sub-seq 0))))
   a-seq))

(defn valid-rows? [board]
  (valid? (rows board)))

(defn cols [board]
  (map
   (fn [col] (col-values board [0 col]))
   (range 9)))

(defn valid-cols? [board]
  (valid? (cols board)))

(defn blocks [board]
  (for [row (range 3)
        col (range 3)]
    (block-values board [(* row 3) (* col 3)])))

(defn valid-blocks? [board]
  (valid? (blocks board)))

(defn valid-solution? [board]
  (and
   (valid-rows? board)
   (valid-cols? board)
   (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [row 0
         col 0]
    (cond
     (and (== row 9) (== col 9))
     '()

     (not (has-value? board [row col]))
     [row col]

     (== col 8)
     (recur (inc row) 0)

     :else
     (recur row (inc col)))))

(defn solve [board]
  (cond
   (and (filled? board) (valid-solution? board))
    board

   (filled? board)
   nil

  :else
   (first (filter (complement empty?)
                  (let [empty-point (find-empty-point board)]
                    (for [n (valid-values-for board empty-point)]
                      (solve (set-value-at board empty-point n))))))))
