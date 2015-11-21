(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get (get board (get coord 0)) (get coord 1)))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (loop [a-row (get board (get coord 0))
         b-row '()]
    (cond
      (empty? a-row) (into #{} (sort (set b-row)))
      (contains? (set b-row) (first a-row)) (recur (rest a-row) b-row)
      :else (recur (rest a-row) (concat b-row (vector (first a-row)))))))

(defn col-values [board coord]
  (loop [n 9
         b-row '()]
    (cond
      (zero? n) (into #{} (sort (set b-row)))
      (contains? (set b-row) (value-at board [(- n 1) (get coord 1)]))
        (recur (- n 1) b-row)
      :else 
        (recur (- n 1) (concat b-row (vector (value-at board [(- n 1) (get coord 1)])))))))

(defn coord-pairs [coords]
  (for [x coords y coords] (vector x y)))

(defn helpedes [numb]
  (cond
    (< numb 3) (vector 0 1 2)
    (> numb 5) (vector 6 7 8)
    :else (vector 3 4 5)))

(defn block-values [board coord]
  (into #{} (distinct (sort (set (for [x (helpedes (get coord 0)) 
                                       y (helpedes (get coord 1))] 
    (value-at board (vector x y))))))))

(defn valid-values-for [board coord]
  (cond
    (not (zero? (value-at board coord))) (into #{} '())
    :else (clojure.set/difference #{1 2 3 4 5 6 7 8 9} (clojure.set/union 
      (block-values board coord) 
      (row-values board coord)
      (col-values board coord)))))

(defn filled? [board]
  (not (contains? (set (for [x '(0 1 2 3 4 5 6 7 8) 
                             y '(0 1 2 3 4 5 6 7 8)] 
    (zero? (value-at board (vector x y))))) true)))

(defn rows [board]
  (for [x '(0 1 2 3 4 5 6 7 8)] (row-values board (vector x 0))))

(defn valid-rows? [board]
  (not (contains? (set (for [x '(0 1 2 3 4 5 6 7 8)] 
    (zero? (count (clojure.set/difference #{1 2 3 4 5 6 7 8 9} 
                    (get (apply vector (rows board)) x)))))) false)))

(defn cols [board]
  (for [x '(0 1 2 3 4 5 6 7 8)] (col-values board (vector 0 x))))

(defn valid-cols? [board]
  (not (contains? (set (for [x '(0 1 2 3 4 5 6 7 8)] 
    (zero? (count (clojure.set/difference #{1 2 3 4 5 6 7 8 9} 
                    (get (apply vector (cols board)) x)))))) false)))

(defn blocks [board]
  (for [x '(0 3 6) y '(0 3 6)] (block-values board (vector x y))))

(defn valid-blocks? [board]
  (not (contains? (set (for [x '(0 1 2 3 4 5 6 7 8)] 
    (zero? (count (clojure.set/difference #{1 2 3 4 5 6 7 8 9} 
                    (get (apply vector (blocks board)) x)))))) false)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board [(get coord 0) (get coord 1)] new-value))

(defn find-empty-point [board]
  (first (filter (fn [a] (if (nil? a) false true))
    (for [x '(0 1 2 3 4 5 6 7 8) 
          y '(0 1 2 3 4 5 6 7 8)]
      (cond
        (zero? (value-at board (vector x y))) (vector x y)
        :else nil)))))

(defn try-values [board]
  (cond 
    (nil? (find-empty-point board)) (if (valid-solution? board) (vector board) false)
    :else 
      (for [z (valid-values-for board (find-empty-point board))]
        (first (filter (fn [x] (if (not (empty? x)) true false))
          (try-values (set-value-at board (find-empty-point board) z)))))))

(defn solve [board]
  (first (filter (fn [x] (not (nil? x))) (try-values board))))
