(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values (set (range 1 10)))

(def value-at get-in)

(defn has-value? [board coord]
  (contains? all-values (value-at board coord)))

(defn row-values [board [n-row _]]
  (set (get board n-row)))

(defn col-values [board [_ n-col]]
  (set (map (fn [row] (get row n-col)) board)))

(defn coord-pairs [coords]
  (for [n-row coords
        n-col coords]
    [n-row n-col]))


(defn block-values [board coord]
  (let [[top left] (map (fn [xi] (* 3 (int (/ xi 3)))) coord)
        same-block-coords (for [n-row (range top (+ 3 top))
                                n-col (range left (+ 3 left))]
                            [n-row n-col])]
    (set (map (fn [coord] (value-at board coord)) same-block-coords))))



(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (row-values board coord) (col-values board coord) (block-values board coord))))

(defn filled? [board]
  (let [all-coords (coord-pairs (range 9))]
    (every? (fn [coord] (has-value? board coord)) all-coords)))

(defn valid? [a-set]
  (= #{} (set/difference all-values a-set)))

(defn rows [board]
  (for [n-row (range 9)]
    (row-values board [n-row 0])))

(defn valid-rows? [board]
  (every? valid? (rows board)))

(defn cols [board]
  (for [n-col (range 9)]
    (col-values board [0 n-col])))

(defn valid-cols? [board]
  (every? valid? (cols board)))

(defn blocks [board]
  (for [n-row '(0 3 6)
        n-col '(0 3 6)]
    (block-values board [n-row n-col])))

(defn valid-blocks? [board]
  (every? valid? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(def set-value-at assoc-in)

(defn find-empty-point [board]
  (let [all-coords (coord-pairs (range 9))]
    (loop [coord (first all-coords)
           rest-coords (rest all-coords)
           res nil]
      (if (empty? rest-coords)
        res
        (recur (first rest-coords)
               (rest rest-coords)
               (if (has-value? board coord)
                 res
                 coord))))))

(defn my-solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      '())
    (let [coord (find-empty-point board)
          valid-values (valid-values-for board coord)]
       (mapcat (fn [valid-value] (my-solve (set-value-at board coord valid-value))) valid-values))))

(defn solve [board]
  (first (my-solve board)))



