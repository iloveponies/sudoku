(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (== (value-at board coord) 0) false true))

(defn row-values [board coord]
  (set (get board (get coord 0))))

(defn col-values [board coord]
  (let [helper (fn [row] (get row (get coord 1)))]
  (set (map helper board))))

(defn coord-pairs [coords]
  (let [helper (fn [x] (map (fn [y] [x y]) coords))]
    (sort (loop [seqq coords
           acc '()]
      (if (empty? seqq) acc (recur (rest seqq) (concat (helper (first seqq)) acc)))))))

(defn block-values [board coord]
  (let [[x y] coord
        helper1 (fn [a b] [(* 3 (int (/ a 3))) (* 3 (int (/ b 3)))])
        firstcoord (helper1 x y)
        [c d] firstcoord
        firstrow (get board c)
        secondrow (get board (inc c))
        thirdrow (get board (inc (inc c)))]
    (set [(get firstrow d) (get firstrow (inc d)) (get firstrow (inc (inc d))) (get secondrow d) (get secondrow (inc d)) (get secondrow (inc (inc d))) (get thirdrow d) (get thirdrow (inc d)) (get thirdrow (inc (inc d)))])))

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{} (set/difference all-values (set/union (row-values board coord) (col-values board coord) (block-values board coord)))))

(defn filled? [board]
  (let [helper (fn [a-seq] (apply concat a-seq))]
    (not (contains? (set (helper board)) 0))))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (not (contains? (set (map (fn [x] (= x all-values)) (rows board))) false)))

(defn cols [board]
  (reverse (loop [acc1 0
         acc2 '()]
    (if (== acc1 9) acc2 (recur (inc acc1) (cons (col-values board [0 acc1]) acc2))))))

(defn valid-cols? [board]
  (not (contains? (set (map (fn [x] (= x all-values)) (cols board))) false)))

(defn blocks [board]
  [(block-values board [0 0]) (block-values board [0 3]) (block-values board [0 6]) (block-values board [3 0]) (block-values board [3 3]) (block-values board [3 6]) (block-values board [6 0]) (block-values board [6 3]) (block-values board [6 6])])

(defn valid-blocks? [board]
  (not (contains? (set (map (fn [x] (= x all-values)) (blocks board))) false)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [a-seq (coord-pairs [0 1 2 3 4 5 6 7 8])]
    (if (empty? a-seq) nil (if (not (has-value? board (first a-seq))) (first a-seq) (recur (rest a-seq))))))


(defn solve [board]
  (">:( liian vaikee"))









