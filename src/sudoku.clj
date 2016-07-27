(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def sud-b
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (set (map
         (fn [row]
           (get row (second coord)))
         board)))

(defn coord-pairs [coords]
  (vec (apply concat (for [number coords]
    (map (fn [other] (vector number other)) coords)))))

(defn top-left-corner [coord]
  (vector
    (* 3 (int (/ (first coord) 3)))
    (* 3 (int (/ (second coord) 3)))))

(defn next-in-block [coord maxX]
  (cond
    (> (inc (first coord)) maxX) (vector (- (first coord) 2) (inc (second coord)))
    :else (vector (inc (first coord)) (second coord))))

(defn get-block-values [out-set in-board in-top-left curr]
  (if (not=
        (top-left-corner curr)
        in-top-left)
    out-set
    (recur
      (conj out-set (value-at in-board curr))
      in-board
      in-top-left
      (next-in-block curr (+ 2 (second in-top-left))))))

(defn block-values [board coord]
  (get-block-values #{} board (top-left-corner coord) (top-left-corner coord)))

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
