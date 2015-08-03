(ns sudoku
  (:require [clojure.set :as set]))

(defn segment-vec [a-vec n]
  (loop [acc   []
         xs    a-vec]
    (cond
      (empty? xs) acc
      (<= (count xs) n) (conj acc xs)
      :else             (recur (conj acc (subvec xs 0 n)) (subvec xs n)))))

(defn row->string [row]
  (let [segs  (segment-vec row 3)
        lanes (mapv (partial apply str) segs)]
    (clojure.string/join "|" lanes)))

(defn board->string [board]
  (let [rows  (mapv row->string board)
        lines (mapv #(str % \newline) rows)
        segs  (segment-vec lines 3)
        lanes (mapv clojure.string/join segs)
        sep   (str "---+---+---" \newline)]
    (clojure.string/join sep lanes)))

(defn print-board [board]
  (println (board->string board)))

(def board identity)

(defn value-at [board coord]
  nil)

(defn has-value? [board coord]
  nil)

(defn row-values [board coord]
  nil)

(defn col-values [board coord]
  nil)

(defn coord-pairs [coords]
  nil)

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
