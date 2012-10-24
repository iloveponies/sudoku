(ns sudoku.core
  (:require [clojure.set :as set]))

(def board identity)
(def corners (#(for [row % col %] [row col]) [0 3 6]))

(defn- block-area [x]
  (range x (+ x 3)))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (== 0 (value-at board coord))))

(defn row-values [board [x]]
  (set (get board x)))

(defn col-values [board [_ y]]
  (set (map #(get % y) board)))

(defn coord-pairs [coords]
  (for [x coords y coords] [x y]))

(defn block-values [board [row col]]
  (let [[[a b]] (filter #(and (<= (first %) row(+ (first %) 2)) (<= (last %) col (+ (last %) 2))) corners)]
    (set (#(for [x (% a) y (% b)] (value-at board [x y])) block-area))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [possible-values (set (range 1 10))
          value-funcs [row-values col-values block-values]
          taken-values (apply set/union (map #(% board coord) value-funcs))]
      (set/difference possible-values taken-values))))

(defn filled? [board]
  (every? #(has-value? board %) (#(for [x % y %] [x y]) (range 9))))

(defn rows [board]
  (map set board))

(defn- valid-? [board f]
  (every? #(== 9 (count %)) (f board)))

(defn valid-rows? [board]
  (valid-? board rows))

(defn cols [board]
  (#(if (some empty? board) [] (concat (vector (set (% first))) (cols (% rest)))) #(map % board)))

(defn valid-cols? [board]
  (valid-? board cols))

(defn blocks [board]
  (map (fn [[a b]]
         (set (#(for [x (% a) y (% b)] (value-at board [x y])) block-area)))
       corners))

(defn valid-blocks? [board]
  (valid-? board blocks))

(defn valid-solution? [board]
  (every? #(% board) [valid-rows? valid-cols? valid-blocks?]))

(defn set-value-at [board [row col] new-value]
  (assoc board row (assoc (get board row) col new-value)))

(defn find-empty-point [board]
  (first (filter #(not (has-value? board %)) (#(for [x % y %] [x y]) (range 9)))))

(defn solve [board]
  nil)
