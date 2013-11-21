(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (== 0 (value-at board coord))))

(defn row-values [board coord]
  (let [[x _] coord]
    (set (get board x))))

(defn col-values [board coord]
  (let [[_ x] coord]
    (let [helper (fn [i b s]
                   (if (== i 8)
                     (conj s (value-at b [i x]))
                     (recur (inc i) b (conj s (value-at b [i x])))))]
    (helper 0 board #{}))))

(defn coord-pairs [coords]
  (vec (for [c1 coords c2 coords]
    (vector c1 c2))))

(defn block-values [board coord]
  (let [[i j] coord]
    (let [x (* 3 (int (/ i 3)))]
      (let [y (* 3 (int (/ j 3)))]
        (let [block (coord-pairs [0 1 2])]
          (let [helper (fn [n b s]
                         (if (== n 8)
                           (conj s (value-at b (vec (map + [x y] (get block n)))))
                           (recur (inc n) b (conj s (value-at b (vec (map + [x y] (get block n))))))))]
            (helper 0 board #{})))))))

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (set/union (row-values board coord)
                                          (col-values board coord)
                                          (block-values board coord)))))

(defn filled? [board]
  (let [helper (fn [i]
                (if (== i 9)
                  true
                  (if (contains? (set (get board i)) 0)
                    false
                    (recur (inc i)))))]
    (helper 0)))

(defn rows [board]
  nil)

(defn valid-rows? [board]
  nil)

(defn cols [board]
  nil)

(defn valid-cols? [board]
  nil)

(defn blocks [board]
  nil)

(defn valid-blocks? [board]
  nil)

(defn valid-solution? [board]
  nil)

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  nil)

(defn solve [board]
  nil)



