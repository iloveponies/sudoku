(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board coord]
  (let [[x] coord]
    (set (get board x))))

(defn col-values [board coord]
  (let [[x y] coord]
    (set (map (fn [el] (get el y)) board))))

(defn coord-pairs [coords]
  (for [c1 coords
        c2 coords]
    [c1 c2]))

(defn block-values [board coord]
  (let [[x y] coord
        bx (* 3 (int (/ x 3)))
        by (* 3 (int (/ y 3)))]
    (set (for [i (range bx (+ 3 bx))
               j (range by (+ 3 by))]
           (get-in board [i j])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (set/union (block-values board coord)
                                          (row-values board coord)
                                          (col-values board coord)))))

(defn filled? [board]
  (letfn [(get-has-value-set [] (set (for [x (range 0 9)
                                           y (range 0 9)]
                                       (has-value? board [x y]))))]
    (not (contains? (get-has-value-set) false))))

(defn rows [board]
  (map (fn [row] (set row)) board))

(defn valid-rows? [board]
  (empty? (filter (fn [a-set] (or (contains? a-set 0) (< (count (set a-set)) 9))) (rows board))))

(defn cols [board]
  (for [y (range 0 9)]
    (set (for [x (range 0 9)]
           (value-at board [x y])))))

(defn valid-cols? [board]
  (empty? (filter (fn [a-set] (or (contains? a-set 0) (< (count (set a-set)) 9))) (cols board))))


(defn blocks [board]
  (for [x (range 0 9 3)
        y (range 0 9 3)]
    (block-values board [x y])))

(defn valid-blocks? [board]
  (empty? (filter (fn [a-set] (or (contains? a-set 0) (< (count (set a-set)) 9))) (blocks board))))

(defn valid-solution? [board]
  (and (valid-blocks? board)
       (valid-cols? board)
       (valid-rows? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board] ;; TODO loop+recur?
  (first (filter (complement nil?)
                 (for [x (range 0 10)
                       y (range 0 10)]
                   (when (= 0 (value-at board [x y]))
                     [x y])))))

(defn solve [board]
  (letfn [(f [board]
             (if (valid-solution? board)
               board
               (let [coords (find-empty-point board)]
                 (for [v (valid-values-for board coords)
                       sol (f (set-value-at board coords v))]
                   sol))))]
    (f board)))
