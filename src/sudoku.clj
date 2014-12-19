(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= (value-at board coord) 0))

(defn row-values [board [r _]]
  (set (get board r)))

(defn col-values [board [_ c]]
  (set (map #(get % c) board)))

(defn coord-pairs [coords]
  (for [x coords y coords]
    [x y]))

(defn top-left [[r c]]
  (letfn [(f [x]
            (* 3 (quot x 3)))]
    [(f r) (f c)]))

(defn block-values [board coord]
  (let [rc0 (top-left coord)]
    (reduce conj #{}
            (map (fn [rc] (value-at board (mapv + rc0 rc)))
                 (coord-pairs [0 1 2])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                    (row-values board coord)
                    (col-values board coord)
                    (block-values board coord))))

(defn filled? [board]
  (not (contains? (reduce set/union #{} (map set board)) 0)))

(defn rows [board]
  (mapv #(row-values board %) (for [r (range 0 9)] [r 0])))

(defn valid-rows? [board]
  (every? #(= % all-values) (rows board)))

(defn cols [board]
  (mapv #(col-values board %) (for [c (range 0 9)] [0 c])))

(defn valid-cols? [board]
  (every? #(= % all-values) (cols board)))

(defn blocks [board]
  (mapv #(block-values board %) (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (every? #(= % all-values) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (drop-while #(not= (value-at board %) 0) (coord-pairs (range 0 9)))))

(defn solve [board]
  (letfn [(solve-helper [sol]
            (if (filled? sol)
              (when (valid-solution? sol)
                [sol])
              (let [p (find-empty-point sol)]
                (for [x (valid-values-for sol p)
                      sol' (solve-helper (set-value-at sol p x))]
                  sol'))))]
    (first (solve-helper board))))
