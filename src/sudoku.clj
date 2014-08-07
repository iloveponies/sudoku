(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board [row col :as coords]]
  (get-in board coords))

(defn has-value? [board [row col :as coords]]
  (pos? (value-at board [row col])))

(defn row-values [board [row _ :as coords]]
  (reduce conj #{} (board row)))

(defn col-values [board [_ col :as coords]]
  (reduce #(conj %1 (%2 col)) #{} board))

(defn coord-pairs [coord-seq]
  (for [row coord-seq col coord-seq]
    [row col]))

(def block-side-length 3)

(defn top-left [v]
 (-> v (quot block-side-length) (* block-side-length)))

(defn block-values [board [row col :as coords]]
  (let [row0 (top-left row) col0 (top-left col)]
    (set (for [row (range row0 (+ row0 block-side-length)) col (range col0 (+ col0 block-side-length))]
      (value-at board [row col])))))

(defn valid-values-for [board [row col :as coords]]
  (if (has-value? board coords)
    #{}
    (set/difference all-values
      (set/union (row-values board coords) (col-values board coords) (block-values board coords)))))

(defn filled? [board]
  (every? pos? (apply concat board)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? #(= % all-values) (rows board)))

(defn cols [board]
  (map #(col-values board [nil %]) (range (count board))))

(defn valid-cols? [board]
  (every? #(= % all-values) (cols board)))

(def board-side-length 9)

(defn blocks [board]
  (let [range (range 0 board-side-length block-side-length)]
    (for [row range col range]
      (block-values board [row col]))))

(defn valid-blocks? [board]
  (every? #(= % all-values) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (some #(if (has-value? board %) nil %) (coord-pairs (range 0 board-side-length))))

(defn sols [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      ())
    (let [coords (find-empty-point board) valid-vals (valid-values-for board coords)]
      (for [val valid-vals sol (sols (set-value-at board coords val))]
        sol))))

(defn solve [board]
  (-> board sols first))
