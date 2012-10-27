(ns sudoku.core
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (= (value-at board coord) 0) false true))

(defn row-values [board coord]
  (let [[x y] coord]
    (set (get board x))))

(defn col-values [board coord]
  (let [[x y] coord
        col-val (fn [a-vec] (nth a-vec y))]
    (set (map col-val board))))

(defn coord-pairs [coords]
  (for [a coords
        b coords]
    [a b]))

(defn block-values [board coord]
  (let [block-coord (fn [coord]
                      (let [[x y] coord
                            x2 (* 3 (int (/ x 3)))
                            y2 (* 3 (int (/ y 3)))]
                        [x2 y2]))
        block-coords (fn [coord]
                       (let [[x y] (block-coord coord)]
                         (for [a (range x (+ x 3))
                               b (range y (+ y 3))]
                           [a b])))]
    (set (map (fn [elem] (value-at board elem)) (block-coords coord)))))

(defn valid-values-for [board coord]
  (let [all-values #{1 2 3 4 5 6 7 8 9}
        vals-for-row (fn [coord]
                       (set/difference all-values (row-values board coord)))
        vals-for-col (fn [coord]
                       (set/difference all-values (col-values board coord)))
        vals-for-block (fn [coord]
                         (set/difference all-values
                                         (block-values board coord)))]
    (if (has-value? board coord)
      #{}
      (set/intersection (vals-for-row coord)
                        (vals-for-col coord)
                        (vals-for-block coord)))))

(defn filled? [board]
  (let [all-vals (fn []
                  (apply set/union 
                         (for [y (range 0 9)]
                           (row-values board [0 y]))))]
    (if (contains? (all-vals) 0) false true)))

(defn rows [board]
  (map (fn [elem] (row-values board elem))
       (for [y (range 0 9)]
         [y 0])))

(defn solve [board]
  ":(")

(defn valid-rows? [board]
  (let [all-values #{1 2 3 4 5 6 7 8 9}]
    (every? (fn [elem] (= elem all-values)) (rows board))))

(defn cols [board]
  (map (fn [elem] (col-values board elem))
       (for [x (range 0 9)]
         [0 x])))

(defn valid-cols? [board]
  (let [all-values #{1 2 3 4 5 6 7 8 9}]
    (every? (fn [elem] (= elem all-values)) (rows board))))

(defn blocks [board]
  (map (fn [elem] (block-values board elem))
       (for [x (range 0 3)
             y (range 0 3)]
         [(* x 3) (* y 3)])))

(defn valid-blocks? [board]
  (let [all-values #{1 2 3 4 5 6 7 8 9}]
    (every? (fn [elem] (= elem all-values)) (blocks board))))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  ((fn [a-seq]
    (cond
     (empty? a-seq)
     nil
     (has-value? board (first a-seq))
     (recur (rest a-seq))
     :else
     (first a-seq)))(for [x (range 0 9)
        y (range 0 9)]
    [x y])))