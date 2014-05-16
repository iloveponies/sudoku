(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (> (value-at board coord) 0))

(defn row-values [board [y x]]
  (loop [values #{}
         a-col (get board y)]
    (if (empty? a-col)
      values
      (recur (conj values (first a-col)) (rest a-col)))))

(defn col-values [board [y x]]
  (loop [values #{}
         a-row board]
    (if (empty? a-row)
      values
      (recur (conj values (get (first a-row) x)) (rest a-row)))))

(defn coord-pairs [coords]
  (for [y coords
        x coords]
    [y x]))

(defn top-left-block-coord [y x]
  (let [round-down (fn [value]
                     (- value (mod value 3)))]
    [(round-down y) (round-down x)]))

(defn block-coords [[y x]]
  (for [j (range 3)
        i (range 3)]
    [(+ j (get (top-left-block-coord y x) 0))
     (+ i (get (top-left-block-coord y x) 1))]))

(defn block-values-perfect [board coords]
  (loop [values #{}
         a-coord (block-coords coords)]
    (if (empty? a-coord)
      values
      (recur (if (has-value? board (first a-coord))
               (conj values (value-at board (first a-coord)))
               values
             ) (rest a-coord)))))

(defn block-values [board coords]
  (loop [values #{}
         a-coord (block-coords coords)]
    (if (empty? a-coord)
      values
      (recur (conj values (value-at board (first a-coord))) (rest a-coord)))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference #{0 1 2 3 4 5 6 7 8 9} (block-values board coord))))

(defn filled? [board]
  (loop [rows board]
    (cond
     (empty? (first rows))
       true
     (some zero? (first rows))
       false
     :else
       (recur (rest rows)))))

(defn rows [board]
  (for [y (range 9)]
    (row-values board [y 0])))

(defn valid-rows? [board]
  nil)

(defn cols [board]
  (for [x (range 9)]
    (col-values board [0 x])))

(defn valid-cols? [board]
  nil)

(defn blocks [board]
  (for [b '([0 0] [0 3] [0 6]
            [3 0] [3 3] [3 6]
            [6 0] [6 3] [6 6])]
    (block-values board b)))

(defn valid-blocks? [board]
  nil)

(defn valid-solution? [board]
  nil)

(defn set-value-at [board coord new-value]
  nil)

(defn find-empty-point [board]
  nil)

(defn solve [board]
  nil)
