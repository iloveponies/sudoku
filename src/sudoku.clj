(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))


(defn has-value? [board coord]
  (if (not= 0 (get-in board coord)) true false))


(defn row-values [board [row col]]
  (let [values (get board row)]
    (loop [values-set #{}
           rest-values values]
      (if (empty? rest-values)
        values-set
        (recur (conj values-set (first rest-values)) (rest rest-values))))))


(defn col-values [board [_ col]]
  (loop [values #{}
         row 0]
    (if (== 9 row)
      values
      (recur (conj values (value-at board [row col])) (inc row)))))


(defn coord-pairs [coords]
  (for [first-i coords
        second-i coords]
    (vector first-i second-i)))

(defn real-coord-pairs [row-coords col-coords]
  (for [row-c row-coords
        col-c col-coords]
    (vector row-c col-c)))


(defn top-left-coord [[row col]]
  (let [coordinate (fn [coord]
                     (cond
                      (<= coord 2) 0
                      (and (<= coord 5) (>= coord 3)) 3
                      :else 6))
        top-row (coordinate row)
        top-col (coordinate col)]
    [top-row top-col]))


(defn block-values [board coord]
  (let [[top-row top-col] (top-left-coord coord)
        block-pairs (real-coord-pairs [top-row (inc top-row) (+ 2 top-row)] [top-col (inc top-col) (+ 2 top-col)])]
    (loop [values #{}
           block-coords block-pairs]
      (if (empty? block-coords)
        values
        (recur (conj values (value-at board (first block-coords))) (rest block-coords))))))



(defn valid-values-for [board coord]
  (let [row-vals (row-values board coord)
        col-vals (col-values board coord)
        block-vals (block-values board coord)]
    (if (has-value? board coord)
      #{}
      (set/difference (set/difference (set/difference all-values row-vals) col-vals) block-vals))))


(defn all-numbers-on-board [board]
  (loop [values #{}
         rows board
         index 0]
    (if (empty? rows)
      values
      (recur (set/union values (row-values board [index 0])) (rest rows) (inc index)))))

(defn filled? [board]
  (let [values (all-numbers-on-board board)]
    (if (contains? values 0) false true)))



(defn rows [board]
  (loop [values []
         rest-board board
         index 0]
    (if (empty? rest-board)
      values
      (recur (conj values (row-values board [index 0])) (rest rest-board) (inc index)))))


(defn cols [board]
  (loop [values []
         rest-board board
         index 0]
    (if (empty? rest-board)
      values
      (recur (conj values (col-values board [0 index])) (rest rest-board) (inc index)))))


(defn blocks-in-a-row [board row]
  (loop [values []
         rest-board board
         index 0]
    (if (== 9 index)
      values
      (recur (conj values (block-values board [row index])) (rest rest-board) (+ 3 index)))))


(defn blocks [board]
  (let [first-row (blocks-in-a-row board 0)
        second-row (blocks-in-a-row board 3)
        third-row (blocks-in-a-row board 6)
        two-rows (reduce conj first-row second-row)]
    (reduce conj two-rows third-row)))


(defn valid-rows? [board]
  (let [all-rows (rows board)]
    (loop [result true
           rest-rows all-rows]
      (if (empty? rest-rows)
        result
        (if (not result)
          false
          (recur (empty? (set/difference all-values (first rest-rows))) (rest rest-rows)))))))



(defn valid-cols? [board]
  (let [all-cols (cols board)]
    (loop [result true
           rest-cols all-cols]
      (if (empty? rest-cols)
        result
        (if (not result)
          false
          (recur (empty? (set/difference all-values (first rest-cols))) (rest rest-cols)))))))



(defn valid-blocks? [board]
  (let [all-blocks (blocks board)]
    (loop [result true
           rest-blocks all-blocks]
      (if (empty? rest-blocks)
        result
        (if (not result)
          false
          (recur (empty? (set/difference all-values (first rest-blocks))) (rest rest-blocks)))))))


(defn valid-solution? [board]
  (let [rows-valid (valid-rows? board)
        cols-valid (valid-cols? board)
        blocks-valid (valid-blocks? board)]
    (if (and rows-valid cols-valid blocks-valid) true false)))



(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))


(defn find-empty-point [board]
  (let [all-rows (rows board)
        first-empty-row (loop [row 0
                               all-rows all-rows]
                          (if (empty? all-rows)
                            nil
                            (if (contains? (first all-rows) 0)
                              row
                              (recur (inc row) (rest all-rows)))))]
    (loop [col 0
           row (get board first-empty-row)]
      (if (empty? row)
        nil
        (if (== 0 (first row))
          [first-empty-row col]
          (recur (inc col) (rest row)))))))




(defn solve [board]
  nil)
