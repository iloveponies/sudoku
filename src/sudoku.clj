(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))


(defn row-values [board coord]
  (let [[y] coord]
    (set (get-in board [y]))))

(defn col-values [board coord]
  (let [[y x] coord]
    (set (map (fn [a] (get-in board [a x]) ) (range 0 9)))))

(defn coord-pairs [coords]
  (let [pairs (for [n coords
                    m coords]
                [n m])]
    (reduce conj [] pairs)))

(defn atcorner [board coord]
  "Helper for block-values"
  (let [[y x] coord
        assigner (fn [a]
                   (cond
                    (>= a 6) 6
                    (>= a 3) 3
                    :else 0))]
    [(assigner y) (assigner x)]))

(defn block-coord-pairs [ycoords xcoords]
  "Helper for block-values"
  (let [pairs (for [n xcoords
                    m ycoords]
                [m n])]
    (reduce conj [] pairs)))




(defn block-values [board coord]
  (let [cornercoords (atcorner board coord)
        [y x] cornercoords
        block-coords (block-coord-pairs
                      (range y (+ 3 y))
                      (range x (+ 3 x)))]
    (set (map (fn [a] (value-at board a)) block-coords))))

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [row (row-values board coord)
          col (col-values board coord)
          block (block-values board coord)
          nonfits (clojure.set/union row col block)]
      (clojure.set/difference all-values nonfits))))

(defn values-in-board [board]
  (let [seq1 (range 0 9)
        vals (map (fn [x] (row-values board [x x])) seq1)]
    (apply clojure.set/union vals)))

(defn filled? [board]
  (empty? (clojure.set/difference (values-in-board board) all-values)))

(defn rows [board]
  (let [seq1 (range 0 9)
        vals (map (fn [y] (row-values board [y y])) seq1)]
    (reduce conj [] vals)))

(defn valid-sets [sets]
  (every? identity (map (fn [x] (empty? (set/difference all-values x))) sets)))

(defn valid-rows? [board]
  (valid-sets (rows board)))



(defn cols [board]
  (let [seq1 (range 0 9)
        vals (map (fn [x] (col-values board [x x])) seq1)]
    (reduce conj [] vals)))

(defn valid-cols? [board]
  (valid-sets (cols board)))

(defn blocks [board]
  (let [seq1 (coord-pairs [0 3 6])
        vals (map (fn [[y x]] (block-values board [y x])) seq1)]
    (reduce conj [] vals)))

(defn valid-blocks? [board]
  (valid-sets (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (some identity (for [y (range 9)
                       x (range 9)]
                   (if (has-value? board [y x])
                     false
                     [y x]))))

(defn solve [board]
  (cond
   (filled? board) (if (valid-solution? board) board '())
   :else (let [empty-pt (find-empty-point board)]
           (for [val (valid-values-for board empty-pt)
                 solved (solve (set-value-at board empty-pt val))]
             solved))))
