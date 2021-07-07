(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[row, _] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[_, col] coord]
    (reduce (fn [cvals row] (conj cvals (get row col))) #{} board)))

(defn coord-pairs [coords]
  (for [c1 coords
        c2 coords]
    [c1 c2]))

(defn which-block [coord]
  (let [helper-fn (fn [x]
                    (cond
                      (< x 3) 0
                      (< x 6) 1
                      :else 2))]
  (map helper-fn coord)))

(defn block-values [board coord]
  (let [topleft (coord-pairs [0 1 2])
        addition (map #(* 3 % 1) (which-block coord))
        helper (fn [z] (map + addition z))
        block-coords (map helper topleft)]
    (reduce (fn [a-set x] (conj a-set (value-at board x))) #{} block-coords)))

(defn valid-values-for [board coord]
  (if (not (zero? (value-at board coord)))
    #{}
    (let [row-vals (row-values board coord)
          col-vals (col-values board coord)
          block-vals (block-values board coord)]
      (clojure.set/difference all-values row-vals col-vals block-vals))))

(defn filled? [board]
  (empty? (filter #{0} (apply concat board))))

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
