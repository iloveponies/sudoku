(ns sudoku
  (:require [clojure.set :as set]))

(def all-values #{1 2 3 4 5 6 7 8 9})

(def board identity)

(defn value-at [board coord]
  (get-in board coord))


(defn has-value? [board coord]
  (not= (value-at board coord) 0))


(defn row-values [board coord]
  (reduce #(conj %1 %2) (set()) (get-in board (conj [] (first coord)))))

(defn col-values [board coord]
  (loop [row 0
         ret-set (set ())]
    (cond
     (= row 9) ret-set
     :else (recur (inc row) (conj ret-set (value-at board (conj (conj [] row) (first (rest coord))))))
    )))

(defn coord-pairs [coords]
    (for [x coords
          y coords]
         (conj (conj [] x) y)))

(defn block-values [board coord]
  nil)

(defn valid-values-for [board coord]
  nil)

(defn filled? [board]
  nil)

(defn rows [board]
  nil)

(defn valid-rows? [board]
  nil)

(defn cols [board]
  nil)

(defn valid-cols? [board]
  nil)

(defn blocks [board]
  nil)

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
