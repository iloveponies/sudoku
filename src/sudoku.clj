(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(def all-idxs (range 0 9))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board [r _]]
  (set (get board r)))

(defn col-values [board [_ c]]
  (reduce #(conj %1 (value-at board [%2 c])) #{} all-idxs))

(defn coord-pairs [coords]
  (for [r coords
        c coords]
    [r c]))

(defn top-left-corner [[r c]]
  [(* (quot r 3) 3) (* (quot c 3) 3)])

(defn bottom-right-corner [[r c]]
  [(* (+ (quot r 3) 1) 3) (* (+ (quot c 3) 1) 3)])

(defn coord-pairs-square [coord]
  (let [[top-left-row top-left-col] (top-left-corner coord)
        [bottom-right-row bottom-right-col] (bottom-right-corner coord)]
    (for [r (range top-left-row bottom-right-row)
          c (range top-left-col bottom-right-col)]
      [r c])))

(defn print-board [board]
  (doseq [row board]
    (println row)))

(defn block-values [board coord]
  (set (map #(value-at board %1) (coord-pairs-square coord))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [row-vals (row-values board coord)
          col-vals (col-values board coord)
          blk-vals (block-values board coord)]
      (clojure.set/difference all-values row-vals col-vals blk-vals))))

(defn filled? [board]
  (every? (partial has-value? board) (coord-pairs all-idxs)))

(defn rows [board]
  (map #(row-values board [%1 nil]) all-idxs))

(defn valid-rows? [board]
  (every? #(= %1 all-values) (rows board)))

(defn cols [board]
  (map #(col-values board [nil %1]) all-idxs))

(defn valid-cols? [board]
  (every? #(= %1 all-values) (cols board)))

(defn blocks [board]
  (for [r (range 0 9 3)
        c (range 0 9 3)]
    (block-values board [r c])))

(defn valid-blocks? [board]
  (every? #(= %1 all-values) (blocks board)))

(defn valid-solution? [board]
  (and (valid-blocks? board) (valid-cols? board) (valid-rows? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (complement (partial has-value? board)) (coord-pairs all-idxs))))

(defn solve [board]
  (cond (valid-solution? board) board
        (filled? board) nil
        :else (let [empty-location (find-empty-point board)
                    vals (valid-values-for board empty-location)
                    try-solution #(solve (set-value-at board empty-location %))]
                (loop [v vals]
                    (if (empty? v)
                      nil
                      (let [solution (try-solution (first v))]
                        (if (boolean solution)
                          solution
                          (recur (rest v)))))))))
