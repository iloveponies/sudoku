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
  (loop [row 0
         coll []]
    (if (nil? (get-in board [row 2]))
      (set coll)
      (recur (inc row) (cons (get-in board [row (get coord 1)]) coll)))))

(defn coord-pairs [coords]
  (loop [seq1 coords
         pairs []
         ind 0
         len (- (count coords) 1)]
    (cond
      (empty? seq1) pairs
      (> ind len) (recur (rest seq1) pairs 0 len)
      :else (recur seq1 (conj pairs [(first seq1) (get coords ind)]) (inc ind) len))))

(defn block-values [board coord]
  (let [helper (fn [coords]
                 (cond
                  (and (< (get coords 0) 3) (< (get coords 1) 3)) [0 0]
                  (and (< (get coords 0) 3) (< (get coords 1) 6)) [0 3]
                  (and (< (get coords 0) 3) (< (get coords 1) 9)) [0 6]
                  (and (< (get coords 0) 6) (< (get coords 1) 3)) [3 0]
                  (and (< (get coords 0) 6) (< (get coords 1) 6)) [3 3]
                  (and (< (get coords 0) 6) (< (get coords 1) 9)) [3 6]
                  (and (< (get coords 0) 9) (< (get coords 1) 3)) [6 0]
                  (and (< (get coords 0) 9) (< (get coords 1) 6)) [6 3]
                  (and (< (get coords 0) 9) (< (get coords 1) 9)) [6 6]))]
    (loop [values []
           x (get (helper coord) 1)
           y (get (helper coord) 0)
           counter 0]
      (cond
        (= counter 12) (set values)
        (< counter 3) (recur (conj values (value-at board [y x])) (inc x) y (inc counter))
        (= counter 3) (recur values (- x 3) y (inc counter))
        (< counter 7) (recur (conj values (value-at board [(+ y 1) x])) (inc x) y (inc counter))
        (= counter 7) (recur values (- x 3) y (inc counter))
        (< counter 11) (recur (conj values (value-at board [(+ y 2) x])) (inc x) y (inc counter))
        :else (set values)
        ))))



(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
  (set/difference all-values (set/union (row-values board coord) (col-values board coord) (block-values board coord)))))

(defn filled? [board]
  (loop [x 0
         y 0]
    (cond
           (= x 9) (recur 0 (inc y))
      (= y 9) true
      (not (has-value? board [y x])) false

      :else (recur (inc x) y))))

(defn rows [board]
  (loop [y 0
         list1 []]
    (if (= y 9)
      list1
      (recur (inc y) (conj list1 (row-values board [y 0]))))))

(defn valid-rows? [board]
  (loop [row (rows board)]
    (cond
      (empty? row) true
      (seq (set/difference all-values (first row))) false
      :else (recur (rest row)))))


(defn cols [board]
    (loop [x 0
         list1 []]
    (if (= x 9)
      list1
      (recur (inc x) (conj list1 (col-values board [0 x]))))))

(defn valid-cols? [board]
  (loop [col (cols board)]
    (cond
      (empty? col) true
      (seq (set/difference all-values (first col))) false
      :else (recur (rest col)))))

(defn blocks [board]
       (loop [x 0
             y 0
         list1 []]
    (cond
      (= y 9) list1
      (= x 9) (recur 0 (+ y 3) list1)
      :else (recur (+ x 3) y (conj list1 (block-values board [y x]))))))

(defn valid-blocks? [board]
   (loop [block (blocks board)]
    (cond
      (empty? block) true
      (seq (set/difference all-values (first block))) false
      :else (recur (rest block)))))

(defn valid-solution? [board]
  (and (valid-cols? board) (valid-rows? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [x 0
         y 0
         coord []]
         (cond
           (= y 9) nil
           (= x 9) (recur 0 y coord)
           (not (has-value? board [y x])) (conj coord y x)
           :else (recur (inc x) y coord))))



(defn solve [board]
  nil)

