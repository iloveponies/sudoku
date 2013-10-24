(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (== 0 (value-at board coord))))

(defn row-values [board [row col]]
  (loop [row-seq (get board row)
         row-set #{}]
    (if (empty? row-seq)
      row-set
      (recur (rest row-seq) (conj row-set (first row-seq))))))

(defn col-values [board [row col]]
  (loop [n 8
         col-set #{}]
    (if (< n 0)
      col-set
      (recur (dec n) (conj col-set (value-at board [n col]))))))

(defn coord-pairs [coords]
  (into [] (for [x coords
                 y coords]
             (vector x y))))

(defn block-coord-pairs [[row col]]
  (let [row-add (* 3 (int (/ row 3)))
        col-add (* 3 (int (/ col 3)))]
    (map (fn [[x y]]
           (vector (+ x row-add) (+ y col-add)))
         (coord-pairs [0 1 2]))))

(defn block-values [board coord]
  (loop [loop-seq (block-coord-pairs coord)
         block-set #{}]
    (if (empty? loop-seq)
      block-set
      (recur (rest loop-seq)
             (conj block-set (value-at board (first loop-seq)))))))

(defn valid-values-for [board coord]
  (if (== 0 (value-at board coord))
    (set/difference all-values (set/union (row-values board coord)
                                          (col-values board coord)
                                          (block-values board coord)))
    #{}))

(defn filled? [board]
  (not (contains?
        (loop [n 8
               res-set #{}]
          (if (< n 0)
            res-set
            (recur (dec n) (set/union res-set (row-values board [n 0])))))
   0)))

(defn rows [board]
  (into [] (for [n (range 0 9)] (row-values board [n 0]))))

(defn valid-sets? [a-seq]
  (let [counterf (fn [n a-set]
                   (if (empty? (set/difference all-values a-set)) (inc n) n))]
    (== 9 (reduce counterf 0 a-seq))))

(defn valid-rows? [board]
  (valid-sets? (rows board)))

(defn cols [board]
  (into [] (for [n (range 0 9)] (col-values board [0 n]))))

(defn valid-cols? [board]
  (valid-sets? (cols board)))

(defn blocks [board]
  (into [] (for [r-i (range 0 3)
                 c-i (range 0 3)]
             (block-values board [(* 3 r-i) (* 3 c-i)]))))

(defn valid-blocks? [board]
  (valid-sets? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [n 80]
    (cond
     (< n 0)
       nil
     (== 0 (value-at board (vector (int (/ n 9)) (mod n 9))))
       (vector (int (/ n 9)) (mod n 9))
     :else
       (recur (dec n)))))

(defn solve-helper [start-b current-b]
  (cond
   (valid-solution? current-b) [current-b]
   (filled? current-b) []
   :else
     (let [p (find-empty-point current-b)
           p-v-seq (valid-values-for current-b p)]
       (for [p-v p-v-seq
             solution (solve-helper start-b (set-value-at current-b p p-v))]
         solution))))

(defn solve [board]
  (first (solve-helper board board)))

; ^___^ -kawaiii
