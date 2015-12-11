(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (= 0 (value-at board coord)) false true))

(defn row-values [board coord]
  (set (map #(value-at board [(coord 0) %]) (range 9)))
  )

(defn col-values [board coord]
  (set (map #(value-at board [% (coord 1)]) (range 9))))

(defn coord-pairs [coords]
  (for [r coords c coords]
    [r c]))

(defn top-left-of-block [coord]
  (let [[r c] coord]
    [(- r (mod r 3)) (- c (mod c 3))]))

(defn block-values [board coord]
  (let [tl (top-left-of-block coord)
        block-coords (map #(map + tl %) (coord-pairs [0 1 2]))]
    (set (map #(value-at board %) block-coords))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference
      all-values
      (block-values board coord)
      (row-values board coord)
      (col-values board coord))))

(defn filled? [board]
  (if (every? true?
              (map #(contains? % 0)
                   (map #(row-values board [% 0]) (range 9))))
    false
    true))

(defn rows [board]
  (map #(row-values board [% 0]) (range 9)))

(defn valid-rows? [board]
  (every? empty?
          (map #(set/difference all-values %) (rows board))))

(defn cols [board]
  (map #(col-values board [0 %]) (range 9)))

(defn valid-cols? [board]
  (every? empty?
          (map #(set/difference all-values %) (cols board))))

(defn blocks [board]
  (for [r (range 0 9 3) c (range 0 9 3)]
    (block-values board [r c])))

(defn valid-blocks? [board]
  (every? empty?
          (map #(set/difference all-values %) (blocks board))))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter #(not (nil? %))
                 (for [r (range 9) c (range 9)]
                   (if (zero? (value-at board [r c]))
                     [r c])))))

(defn solve [board]
  (if (valid-solution? board)
    board
    (let [coord (find-empty-point board)
          remaining (valid-values-for board coord)]
      (for [pt remaining solution (solve (set-value-at board coord pt))]
        solution))))
