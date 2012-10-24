(ns sudoku.core
  (:require [clojure.set :as set]))

(def board identity)
(def corners (#(for [row % col %] [row col]) [0 3 6]))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (== 0 (value-at board coord))))

(defn row-values [board [x]]
  (set (get board x)))

(defn col-values [board [_ y]]
  (set (map #(get % y) board)))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-values [board [row col]]
  (let [[[a b]] (filter #(and (<= (first %) row(+ (first %) 2)) (<= (last %) col (+ (last %) 2))) corners)]
    (set (for [x (range a (+ a 3))
               y (range b (+ b 3))]
           (value-at board [x y])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [possible-values (set (range 1 10))
          value-funcs [row-values col-values block-values]
          taken-values (apply set/union (map #(% board coord) value-funcs))]
      (set/difference possible-values taken-values))))

(defn filled? [board]
  (every? #(has-value? board %) (for [x (range 9) y (range 9)] [x y])))

(defn rows [board]
  (map set board))

(defn- valid-? [board f]
  (every? #(== 9 (count %)) (f board)))

(defn valid-rows? [board]
  (valid-? board rows))

(defn cols [board]
  (if (some empty? board)
    []
    (concat (vector (set(map first board)))
            (cols (map rest board)))))

(defn valid-cols? [board]
  (valid-? board cols))

(defn blocks [board]
  (map (fn [[a b]]
         (set (for [x (range a (+ a 3))
                    y (range b (+ b 3))]
                (value-at board [x y]))))
       corners))

(defn valid-blocks? [board]
  (valid-? board blocks))

(defn valid-solution? [board]
  (every? #(% board) [valid-rows? valid-cols? valid-blocks?]))

(defn set-value-at [board coord new-value]
  nil)

(defn find-empty-point [board]
  nil)

(defn solve [board]
  nil)
