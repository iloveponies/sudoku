(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (let [value (value-at board coord)]
    (if (== 0 value)
      false
      true)))

(defn row-values [board coord]
  (let [row (get board (first coord))]
    (loop [acc #{}
           r row]
      (if (empty? r)
        acc
        (recur (conj acc (first r)) (rest r))))))

(defn col-values [board coord]
  (let [helper (fn [col brd]
                 (if (nil? brd)
                   col
                   (conj col (get brd (second coord)))))]
    (reduce helper #{} board)))

(defn coord-pairs
  ([coords]
   (for [x coords
        y coords]
    [x y]))
  ([row col]
   (for [x row
         y col]
     [x y])))

(defn block-values [board coord]
  (let [get-corner (fn [c]
                     (cond (< c 3) 0
                     (< 2 c 6) 3
                     (<= 6 c) 6))]
    (let [corner [(get-corner (first coord)) (get-corner (last coord))]]
      (let [row (range (first corner) (+ 3 (first corner)))
            col (range (second corner) (+ 3 (second corner)))]
        (let [coords (coord-pairs row col)]
          (loop [acc #{}
                 coor coords]
            (if (empty? coor)
              acc
              (recur (conj acc (value-at board (first coor))) (rest coor)))))))))

(defn valid-values-for [board coord]
  (if (== 0 (value-at board coord))
    (set/difference all-values (set/union (block-values board coord)
                                        (col-values board coord)
                                        (row-values board coord)))
    #{}))

(defn filled? [board]
  (loop [acc #{}
         rows board]
    (if (empty? rows)
      (not (contains? acc 0))
      (recur (set/union acc (set (first rows))) (rest rows)))))

(defn rows [board]
  (loop [acc []
         row 0]
    (if (< row 9)
      (recur (conj acc (row-values board [row 0])) (inc row))
      acc)))

(defn valid-chunk? [chunk]
  (loop [acc #{}
         all chunk]
    (cond (contains? acc (first all)) false
          (empty? all) true
          :else (recur (conj acc (first all)) (rest all)))))

(defn valid-rows? [board]
  (loop [all-rows board]
    (cond (false? (valid-chunk? (first all-rows))) false
          (empty? all-rows) true
          :else (recur (rest all-rows)))))

(defn cols [board]
  (loop [acc []
         col 0]
    (if (< col 9)
      (recur (conj acc (col-values board [0 col])) (inc col))
      acc)))

(defn valid-cols? [board]
  (let [column (fn [col]
                 (loop [acc []
                        row 0]
                   (if (== row 8)
                     (conj acc (value-at board [row col]))
                     (recur (conj acc (value-at board [row col])) (inc row)))))]
    (loop [col 0]
      (cond (== 9 col) true
            (false? (valid-chunk? (column col))) false
            :else (recur (inc col))))))

(defn blocks [board]
  (loop [acc []
         row 0
         col 0]
    (cond (== 6 row col) (conj acc (block-values board [row col]))
          (== 6 col) (recur (conj acc (block-values board [row col])) (+ 3 row) 0)
          :else (recur (conj acc (block-values board [row col])) row (+ 3 col)))))

(defn valid-blocks? [board]
  (let [block (fn [row col]
                (loop [values []
                       coords (coord-pairs (range row (+ row 3)) (range col (+ col 3)))]
                  (if (empty? coords)
                    values
                    (recur (conj values (value-at board (first coords))) (rest coords)))))]
    (loop [row 0
           col 0]
      (cond (== 6 row col) (valid-chunk? (block row col))
            (false? (valid-chunk? (block row col))) false
            (== 6 col) (recur (+ 3 row) 0)
            :else (recur row (+ 3 col))))))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [all (rows board)
         row 0]
    (if (empty? all)
      nil
      (if (contains? (first all) 0)
        (loop [current (first all)
               col 0]
          (if (== 0 (value-at board [row col]))
            [row col]
            (recur (rest current) (inc col))))
        (recur (rest all) (inc row))))))

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      [])
    (let [empty-loc (find-empty-point board)]
      (for [options (valid-values-for board empty-loc)
            solution (solve-helper (set-value-at board empty-loc options))]
        solution))))

(defn solve [board]
  (let [solution (solve-helper board)]
    (if (seq? solution) (first solution)
      solution)))
