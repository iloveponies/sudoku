(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (zero? (value-at board coord))
    false
    true))

(defn row-values [board coord]
  (set (distinct (get board (get coord 0)))))

(defn col-values [board [row col]]
  (loop [index 0
         acc #{}
         location (vector index col)]
    (if (> index 9)
      acc
      (recur (inc index) (conj acc (value-at board location)) (vector index col)))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    (vector row col)))

(defn top-left [i]
  (let [box-1-bound 2
        box-2-bound 5
        box-3-bound 8]
    (cond
     (<= i box-1-bound) 0
     (<= i box-2-bound) 3
     (<= i box-3-bound) 6)))

(defn block-values [board coord]
  (let [top (top-left (get coord 0))
        left (top-left (get coord 1))
        top-vec [top (inc top) (inc (inc top))]
        left-vec [left (inc left) (inc (inc left))]
        end-set #{}]
    (set (for [row top-vec
               col left-vec]
      (value-at board (vector row col))))))

(defn valid-values-for [board coord]
  (if (= 0 (value-at board coord))
    (set/difference all-values (set/union (row-values board coord) (col-values board coord) (block-values board coord)))
    #{}))

(defn all-numbers [board]
  (for [row all-values
        col all-values]
    (value-at board (vector (dec row) (dec col)))))

(defn filled? [board]
  (if (some zero? (all-numbers board))
    false
    true))

(defn rows [board]
  (for [row all-values]
    (row-values board (vector (dec row) 0))))

(defn valid-rows? [board]
  (loop [index 0
         all-row-values (rows board)]
    (cond
      (= index 9) true
      (= (count (nth all-row-values index)) 9) (recur (inc index) (rows board))
      :else false)))

(defn cols [board]
  (for [col all-values]
    (col-values board (vector 0 (dec col)))))

(defn valid-cols? [board]
  (loop [index 0
         all-col-values (cols board)]
    (cond
      (= index 9) true
      (= (count (nth all-col-values index)) 9) (recur (inc index) (cols board))
      :else false)))

(defn blocks [board]
  (remove nil?
    (for [row all-values
          col all-values]
      (if (and (or (= row 1) (= row 4) (= row 7)) (or (= col 1) (= col 4) (= col 7)))
        (block-values board (vector row col))))))

(defn valid-blocks? [board]
  (loop [index 0
         all-block-values (blocks board)]
    (cond
      (= index 9) true
      (= (count (nth all-block-values index)) 9) (recur (inc index) (blocks board))
      :else false)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first
   (remove nil?
    (for [row all-values
          col all-values]
      (if (= 0 (value-at board (vector (dec row) (dec col))))
        (vector (dec row) (dec col))
        nil)))))

(defn solve-helper [board]
  (let [empty-point (find-empty-point board)]
    (if (nil? empty-point)
      (if (valid-solution? board)
        [board]
        ())
      (for [value (valid-values-for board empty-point)
            solution (solve-helper (set-value-at board empty-point value))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))
