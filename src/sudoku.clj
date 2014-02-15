(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board coord]
  (let [row (first coord)] 
    (loop [col 0
           acc #{}]
      (if (= 9 col)
        acc
        (recur (inc col) 
               (conj acc (value-at board [row col])))))))

(defn col-values [board coord]
  (let [col (get coord 1)] 
    (loop [row 0
           acc #{}]
      (if (= 9 row)
        acc
        (recur (inc row) 
               (conj acc (value-at board [row col])))))))

(defn coord-pairs 
  ([coords]
    (for [row coords
          col coords]
      [row col]))
  ([top-row left-col n]
    (for [x (range n)
          y (range n)]
      [(+ top-row x) (+ left-col y)])))

(defn- block-beginning [row-or-col]
  (* 3 (int (/ row-or-col 3))))

(defn- block-upper-left-corner [coord]
  [(block-beginning (first coord)) (block-beginning (get coord 1))])

(defn block-values [board coord]
  (let [[block-top block-left] (block-upper-left-corner coord)]
    (reduce (fn [acc c] (conj acc (value-at board c)))
            #{} 
            (coord-pairs block-top block-left 3))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values 
                    (row-values board coord) 
                    (col-values board coord) 
                    (block-values board coord))))

(defn filled? [board]
  (let [board-as-seq (for [x (range 1 10)
                           y (range 1 10)]
                       (value-at board [x y]))]
    (not (contains? (set board-as-seq) 0))))

(defn rows [board]
  (for [row (range 0 9)]
    (row-values board [row 0])))

(defn- all-valid? [row-col-or-block]
  (every? (fn [s] (empty? (set/difference all-values s))) row-col-or-block))

(defn valid-rows? [board]
  (all-valid? (rows board)))

(defn cols [board]
  (for [col (range 0 9)]
    (col-values board [0 col])))

(defn valid-cols? [board]
  (all-valid? (cols board)))

(defn blocks [board]
  (for [x (range 0 3)
        y (range 0 3)]
    (block-values board [(* 3 x) (* 3 y)])))

(defn valid-blocks? [board]
  (all-valid? (blocks board)))

(defn valid-solution? [board]
  nil)

(defn set-value-at [board coord new-value]
  nil)

(defn find-empty-point [board]
  nil)

(defn solve [board]
  nil)
