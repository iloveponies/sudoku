(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (loop
    [a-set #{}
     n 0]
    (if (== 8 n) (conj a-set (value-at board [(first coord) n]))
    (recur (conj a-set (value-at board [(first coord) n])) (inc n)))))

(defn col-values [board coord]
  (loop
    [a-set #{}
     n 0]
    (if (== 8 n) (conj a-set (value-at board [n (last coord)]))
    (recur (conj a-set (value-at board [n (last coord)])) (inc n)))))

(defn coord-pairs [coords]
  (for [c1 coords
        c2 coords]
    [c1 c2]))

(defn block-values-helper [coord]
  [(- (first coord) (mod (first coord) 3) )
   (- (last coord) (mod (last coord) 3))])

(defn coord-three-next [board coord]
  (conj #{}
        (value-at board coord)
        (value-at board [(first coord) (inc (last coord))])
        (value-at board [(first coord) (+ (last coord) 2)])))

(defn block-values [board coord]
  (let [c (block-values-helper coord)]
    (clojure.set/union
     (coord-three-next board c)
     (coord-three-next board [(inc (first c)) (last c)])
     (coord-three-next board [(+ (first c) 2) (last c)]))))

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
    (set/difference all-values
                    (clojure.set/union
                     (row-values board coord)
                     (col-values board coord)
                     (block-values board coord)))))

(defn filled-helper [board]
  (loop
    [a-set #{}
     n 0]
    (if (== 9 n) a-set
    (recur (clojure.set/union (row-values board [n 0])) (inc n)))))

(defn filled? [board]
  (not (contains? (filled-helper board) 0)))

(defn rows [board]
  (loop
    [a-seq []
     n 0]
    (if (== 9 n) a-seq
      (recur (conj a-seq (row-values board [n 0])) (inc n)))))

(defn valid-rows? [board]
  (every? (fn [x] (empty? (set/difference all-values x))) (rows board)))

(defn cols [board]
  (loop
    [a-seq []
     n 0]
    (if (== 9 n) a-seq
      (recur (conj a-seq (col-values board [0 n])) (inc n)))))

(defn valid-cols? [board]
  (every? (fn [x] (empty? (set/difference all-values x))) (cols board)))

(defn blocks-helper []
  [[0 0] [0 3] [0 6]
   [3 0] [3 3] [3 6]
   [6 0] [6 3] [6 6]])

(defn blocks [board]
  (loop
    [a-seq []
     b-seq (blocks-helper)]
    (if (empty? b-seq) a-seq
      (recur (conj a-seq (block-values board (first b-seq))) (rest b-seq)))))

(defn valid-blocks? [board]
  (every? (fn [x] (empty? (set/difference all-values x))) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop
    [x 0
     y 0]
    (cond
     (== 9 y) nil
     (== 9 x) (recur 0 (inc y))
     (zero? (value-at board [y x])) [y x]
     :else (recur (inc x) y))))


(defn solve [board]
  (if
    (filled? board)
    (if
      (valid-solution? board)
      board
      [])
    (let
      [coord (find-empty-point board)]
      (for
        [n (valid-values-for board coord)
         solution (solve (set-value-at board coord n))]
        solution))))





