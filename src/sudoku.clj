(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (== (get-in board coord) 0)
     false
     true))

(defn row-values [board coord]
  (let [[x y] coord]
    (set (get board x))))

(defn col-values [board coord]
  (let [[x y] coord]
    (loop [seq1 board
           result #{}]
      (if (empty? seq1)
        result
        (recur (rest seq1) (conj result (get (first seq1) y)))))))

(defn coord-pairs [coords]
  (for [n1 coords
        n2 coords]
    [n1 n2]))

(defn block-coord [coord]
  (let [[x y] coord]
    (cond
      (and (<= x 2) (<= y 2)) [0 0]
      (and (<= x 5) (<= y 2)) [3 0]
      (and (<= x 8) (<= y 2)) [6 0]
      (and (<= x 2) (<= y 5)) [0 3]
      (and (<= x 2) (<= y 8)) [0 6]
      (and (<= x 5) (<= y 5)) [3 3]
      (and (<= x 8) (<= y 5)) [6 3]
      (and (<= x 5) (<= y 8)) [3 6]
      (and (<= x 8) (<= y 8)) [6 6])))

(defn block-values [board coord]
  (let [additions (coord-pairs [0 1 2])]
    (set (map (fn [y] (value-at board y)) (map (fn [x] (map + x (block-coord coord))) additions)))))

(defn valid-values-for [board coord]
  (if (== (value-at board coord) 0)
    (set/difference all-values (set/union (row-values board coord) (set/union (col-values board coord) (block-values board coord))))
    #{}))

(defn filled? [board]
  (loop [seq1 board]
    (if (empty? seq1)
      true
      (if (some #{0} (first seq1))
        false
        (recur (rest seq1))))))

(defn rows [board]
  (loop [seq1 board
         result []]
    (if (empty? seq1)
      result
      (recur (rest seq1) (conj result (set (first seq1)))))))

(defn valid? [a-seq]
  (loop [seq1 a-seq]
    (if (empty? seq1)
      true
      (if (== (count (first seq1)) 9)
        (recur (rest seq1))
        false))))

(defn valid-rows? [board]
  (valid? (rows board)))

(defn cols [board]
  (let [one-col (fn [y]
                  (loop [seq1 board
                         result []]
                    (if (empty? seq1)
                      result
                      (recur (rest seq1) (conj result (get (first seq1) y))))))]
    [(set (one-col 0)) (set (one-col 1)) (set (one-col 2)) (set (one-col 3)) (set (one-col 4)) (set (one-col 5)) (set (one-col 6)) (set (one-col 7)) (set (one-col 8))]))

(defn valid-cols? [board]
  (valid? (cols board)))

(defn blocks [board]
  [(block-values board [0 0]) (block-values board [0 3]) (block-values board [0 6]) (block-values board [3 0]) (block-values board [3 3]) (block-values board [3 6]) (block-values board [6 0]) (block-values board [6 3]) (block-values board [6 6])])

(defn valid-blocks? [board]
  (valid? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [pairs (coord-pairs [0 1 2 3 4 5 6 7 8])]
    (if (empty? pairs)
      nil
      (let [coord (first pairs)]
        (if (== (value-at board coord) 0)
          coord
          (recur (rest pairs)))))))

(defn solve-helper [current-set solution]
  (if (filled? solution)
    (if (valid-solution? solution)
      [solution]
      '())
    (let [empty-point (find-empty-point solution)]
      (let [remaining (set/intersection current-set (valid-values-for solution empty-point))]
        (for [elem remaining
              s1 (solve-helper current-set (set-value-at solution empty-point elem))]
          s1)))))


(defn solve [board]
  (first (solve-helper #{1 2 3 4 5 6 7 8 9} board)))
