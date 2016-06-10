(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [r c]]
  (set (get board r)))

(defn col-values [board [r c]]
  (reduce (fn [res elem] (conj res (get elem c))) #{} board))

(defn coord-pairs [coords]
  (for [row coords
        coll coords]
        (conj '() coll row)))

(defn block-top-left [[r c]]
  (conj '() (* 3 (quot c 3)) (* 3 (quot r 3))))

(defn block-values [board coord]
  (let [[r c] (block-top-left coord)
        coords (for [row (range r (+ 3 r))
                     col (range c (+ 3 c))]
                     (conj '() col row))]
    (reduce (fn [res elem] (conj res (get-in board elem))) #{} coords)))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (block-values board coord) (col-values board coord) (row-values board coord))))

(defn filled? [board]
  (let [coords (for [row (range 0 9)
                     col (range 0 9)]
                  (conj '() col row))
        values (reduce (fn [res elem] (conj res (value-at board elem))) #{} coords)]
    (not (contains? values 0))))

(defn rows [board]
  (reduce (fn [res r] (conj res (row-values board [r 0]))) '() (range 8 -1 -1)))

(defn valid-filled? [board elem-fn]
  (reduce 
    (fn [res r] 
      (if (not res)
        res
        (if (contains? r 0)
          true
          (empty? (set/difference all-values r)))))
    true
    (elem-fn board)))

(defn valid-rows? [board]
  (valid-filled? board rows))

(defn cols [board]
  (reduce (fn [res c] (conj res (col-values board [0 c]))) '() (range 8 -1 -1)))

(defn valid-cols? [board]
  (valid-filled? board cols))

(defn blocks [board]
  (reduce (fn [res [r c]] (conj res (block-values board [r c]))) '() (for [row (range 6 -1 -3)
                                                                           col (range 6 -1 -3)]
                                                                        (conj '() col row))))

(defn valid-blocks? [board]
  (valid-filled? board blocks))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [coords (for [row (range 0 9)
                     col (range 0 9)]
                 (conj '() col row))]
    (first (drop-while (fn [coord] (has-value? board coord)) coords))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      '())
    (let [point (find-empty-point board)
          values (valid-values-for board point)]
          (first (drop-while empty? 
                   (reduce (fn [res v] (conj res (solve (set-value-at board point v)))) '() values))))))
