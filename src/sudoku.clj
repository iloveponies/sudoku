(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (== 0 (value-at board coord))))

(defn row-values [board coord]
  (let [[x y] coord]
    (set (get board x))))

(defn col-values [board coord]
  (let [[x y] coord]
    (loop [result (set ())
           index 0]
      (if (< index 9)
        (recur (conj result (value-at board [index y])) (inc index))
        result))))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-top-left [coord]
  (let [[x y] coord]
    [(- x (mod x 3)) (- y (mod y 3))]))

(defn block-values [board coord]
  (let [top-left (block-top-left coord)
        index-to-coord (fn [index] [(quot index 3) (mod index 3)])]
    (loop [result (set ())
          index 0]
      (if (== index 9)
        result
        (recur (conj result (value-at board (map + top-left (index-to-coord index)))) (inc index))))))

(defn valid-values-for [board coord]
  (if (not (== 0 (value-at board coord)))
    (set ())
    (set/difference all-values (block-values board coord) (row-values board coord) (col-values board coord))))

(defn all-board-values [board]
  (loop [result (set ())
         index 0]
    (if (== index 9)
      result
      (recur (set/union result (row-values board [index 0])) (inc index)))))

(defn filled? [board]
  (not (contains? (all-board-values board) 0)))

(defn rows [board]
  (loop [result []
         index 0]
    (if (== index 9)
      result
      (recur (conj result (row-values board [index 0])) (inc index)))))

(defn valid-rows? [board]
  (loop [elem (first (rows board))
         seq (rest (rows board))
         index 0]
    (cond 
      (== index 9)
        true
      (not (== 9 (count elem)))
        false
      :else
        (recur (first seq) (rest seq) (inc index)))))

(defn cols [board]
  (loop [result []
         index 0]
    (if (== index 9)
      result
      (recur (conj result (col-values board [0 index])) (inc index)))))

(defn valid-cols? [board]
  (loop [elem (first (cols board))
         seq (rest (cols board))
         index 0]
    (cond 
      (== index 9)
        true
      (not (== 9 (count elem)))
        false
      :else
        (recur (first seq) (rest seq) (inc index)))))

(defn blocks [board]
  (let [index-to-coord (fn [index] 
                        [(quot index 3) (mod index 9)])]
    (loop [result []
           index 0]
      (if (== index 27)
        result
        (recur (conj result (block-values board (index-to-coord index))) (+ index 3))))))

(defn valid-blocks? [board]
  (loop [elem (first (blocks board))
         seq (rest (blocks board))
         index 0]
    (cond 
      (== index 9)
        true
      (not (== 9 (count elem)))
        false
      :else
        (recur (first seq) (rest seq) (inc index)))))

(defn valid-solution? [board]
  (and (valid-cols? board) (valid-rows? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [index-to-coord (fn [index] 
                        [(quot index 9) (mod index 9)])]
    (loop [index 0]
      (cond 
        (== index 81)
          nil
        (== 0 (value-at board (index-to-coord index)))
          (index-to-coord index)
        :else
          (recur (inc index))))))

(defn solve [board]
  (let [index (find-empty-point board)]
    (cond
      (= index nil)
        (if (valid-solution? board)
          board
          ())
      :else
        (for [elem (valid-values-for board index)
              candidate (solve (set-value-at board index elem))]
          candidate))))
