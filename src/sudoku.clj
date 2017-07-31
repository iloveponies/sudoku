(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (== 0 (value-at board coord))))

(defn row-values [board coord]
  (let [[y x] coord]
    (set (get-in board [y]))))

(defn col-values [board coord]
  (let [[y x] coord]
    (loop [result #{} acc 0]
      (cond
        (== acc (count board))
          result
        :else (recur (conj result (value-at board [acc x])) (inc acc))))))

(defn coord-pairs [coords]
  (let [result (for [a coords b coords]
    [a b])]
      (vec result)))

(defn top-left-coord [coord]
  (let [[y x] coord]
    [(- y (rem y 3)) (- x (rem x 3))]))

(defn block-values [board coord]
  (let [[y x] (top-left-coord coord)]
    (loop [result #{} y-coord y x-coord x]
      (cond
        (== (+ y 3) y-coord)
          result
        (== (+ x 3) x-coord)
          (recur result (inc y-coord) x)
        :else (recur (conj result (value-at board [y-coord x-coord])) y-coord (inc x-coord))))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [taken-values (set/union (row-values board coord)
                                  (col-values board coord)
                                  (block-values board coord))]
      (set/difference all-values taken-values))))

(defn get-all-numbers [board]
  (loop [result '() rows board]
    (if (empty? rows)
      result
      (recur (concat result (first rows)) (rest rows)))))

(defn filled? [board]
  (not (contains? (set (get-all-numbers board)) 0)))

(defn rows [board]
  (loop [result [] rows board]
    (if (empty? rows)
      result
    (recur (conj result (set (first rows))) (rest rows)))))

(defn valid-rows? [board]
  (loop [all-rows (rows board)]
    (cond
      (empty? all-rows)
        true
      (= (first all-rows) all-values)
        (recur (rest all-rows))
      :else false)))

(defn cols [board]
  (loop [result [] acc 0]
    (cond
      (== 9 acc)
        result
      :else (recur (conj result (col-values board [0 acc])) (inc acc)))))

(defn valid-cols? [board]
  (loop [all-cols (cols board)]
    (cond
      (empty? all-cols)
        true
      (= (first all-cols) all-values)
        (recur (rest all-cols))
      :else false)))

(defn blocks [board]
  (loop [result [] y 0 x 0]
    (cond
      (>= x 9)
        result
      (>= y 9)
        (recur result 0 (+ x 3))
      :else (recur (conj result (set (block-values board [x y]))) (+ y 3) x))))

(defn valid-blocks? [board]
  (loop [all-blocks (blocks board)]
    (cond
      (empty? all-blocks)
        true
      (= (first all-blocks) all-values)
        (recur (rest all-blocks))
      :else false)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [y 0 x 0]
    (cond
      (== 9 y)
        nil
      (== 0 (get-in board [y x]))
        [y x]
      (== 8 x)
        (recur (inc y) 0)
      :else (recur y (inc x)))))

(defn solve [board]
  nil)
