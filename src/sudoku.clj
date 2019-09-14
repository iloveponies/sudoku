(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (get-in board coord))))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (set (loop [acc []
                row-list board]
           (if (empty? row-list)
             acc
             (recur (conj acc (get (first row-list) col)) (rest row-list)))))))

(defn coord-pairs [coords]
  (let [[row col] coords]
    (for [i coords
          j coords]
      [i j])))

(defn block-values [board coord]
  (let [[row col] coord
        [top left] [(* 3 (int (/ row 3))) (* 3 (int (/ col 3)))]]
    (loop [acc #{}
           block-indices (for [row-index (range top (+ 3 top))
                               col-index (range left (+ 3 left))]
                           [row-index col-index])]
      (if (empty? block-indices)
        acc
        (recur (conj acc (value-at board (first block-indices))) (rest block-indices))))))

(defn valid-values-for [board coord]
  (let [used-values (set/union (set/union (row-values board coord)
                                          (col-values board coord))
                               (block-values board coord))]
    (if (zero? (value-at board coord))
      (set/difference all-values used-values)
      #{})))

(defn filled? [board]
  (loop [board-seq (apply concat board)]
    (cond
     (empty? board-seq) true
     (zero? (first board-seq)) false
     :else (recur (rest board-seq)))))

(defn rows [board]
  (loop [acc []
         indices (range 0 9)]
    (if (empty? indices)
      acc
      (let [cur-index (first indices)]
        (recur (conj acc (row-values board [cur-index cur-index])) (rest indices))))))

(defn valid-rows? [board]
  (loop [row-values (rows board)]
    (cond
     (empty? row-values) true
     (not= all-values (first row-values)) false
     :else (recur (rest row-values)))))

(defn cols [board]
  (loop [acc []
         indices (range 0 9)]
    (if (empty? indices)
      acc
      (let [cur-index (first indices)]
        (recur (conj acc (col-values board [cur-index cur-index])) (rest indices))))))


(defn valid-cols? [board]
  (loop [col-values (cols board)]
    (cond
     (empty? col-values) true
     (not= all-values (first col-values)) false
     :else (recur (rest col-values)))))

(defn blocks [board]
  (loop [acc []
         indices (for [rows (range 0 9 3)
                       cols (range 0 9 3)]
                   [rows cols])]
    (if (empty? indices)
      acc
      (recur (conj acc (block-values board (first indices))) (rest indices)))))

(defn valid-blocks? [board]
    (loop [block-values (blocks board)]
    (cond
     (empty? block-values) true
     (not= all-values (first block-values)) false
     :else (recur (rest block-values)))))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [indices (for [row (range 0 9)
                       col (range 0 9)]
                   [row col])]
    (cond
     (empty? indices) nil
     (zero? (value-at board (first indices))) (first indices)
     :else (recur (rest indices)))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      [])
    (let [free-point (find-empty-point board)
          valid-values (valid-values-for board free-point)]
      (for [valid-value valid-values
           solution (solve (set-value-at board free-point valid-value))]
        solution))))

