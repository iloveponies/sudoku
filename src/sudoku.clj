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

(defn top-left-coords [coords]
  [(* (int (/ (first coords) 3)) 3) (* (int (/ (first (rest coords)) 3)) 3)])


	  (defn block-values [board coord]
  (let [startY (first (top-left-coords coord))
        startX (first (rest (top-left-coords coord)))]
  (loop [ret-set '()
         curY startY
          curX startX]
    (cond
     (and (= (- curY startY) 2) (= (- curX startX) 2)) (set (conj ret-set (value-at board [curY curX])))
      (= (- curY startY) 2) (recur (conj ret-set (value-at board [curY curX])) startY (inc curX))
      :else (recur (conj ret-set (value-at board [curY curX])) (inc curY) curX)))))

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
