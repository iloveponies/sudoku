(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [row _]]
  (into #{} (get board row)))

(defn col-values [board [_ col]]
  (into #{} (map (fn [x] (value-at board [x col])) (range 9))))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-values [board [row col]]
  (let [x (- row (mod row 3))
        y (- col (mod col 3))
        values (for [x' (range x (+ x 3))
                     y' (range y (+ y 3))]
                 (value-at board [x' y']))]
    (into #{} values)))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                    (block-values board coord)
                    (col-values board coord)
                    (row-values board coord))))

(defn filled? [board]
  (not (every? identity
               (map (fn [row]
                      (contains? (into #{} row) 0))
                     board))))

(defn valid-sets? [xss]
  (every?
    identity
    (map (fn [xs] (empty? (set/difference all-values xs))) xss)))

(defn rows [board]
  (map (partial into #{}) board))

(defn valid-rows? [board]
  (valid-sets? (rows board)))

(defn cols [board]
  (map (fn [x] (col-values board [nil x])) (range 9)))

(defn valid-cols? [board]
  (valid-sets? (cols board)))

(defn blocks [board]
  (let [corners (for [x (range 3) 
                      y (range 3)] 
                  [(* x 3) (* y 3)])]
    (map (fn [coord] (block-values board coord)) corners)))

(defn valid-blocks? [board]
  (valid-sets? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-points [board]
  (take 1 (for [x (range 9) y (range 9)
                :while (not (has-value? board [x y]))]
             [x y])))

(defn find-empty-point [board]
  (first (find-empty-points board)))

(defn solve [board]
  nil)
