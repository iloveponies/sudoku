(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board [r _]]
  (set (get board r)))

(defn col-values [board [r c]]
  (set (map #(get % c) board)))

(defn coord-pairs 
  ([coord] (coord-pairs coord coord))
  ([coord-1 coord-2] (for [x coord-1 y coord-2] [x y])))

(defn- corner [coord]
  (map #(- % (mod % 3)) coord))

(defn- me-and-three-after [x]
  (range x (+ 3 x)))

(defn block-values [board coord]
  (let [[x y] (corner coord)
        coords (coord-pairs (me-and-three-after x) (me-and-three-after y))]
    (set (map #(value-at board %) coords))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [all-values (set (range 1 10))
          rv (row-values board coord)
          cv (col-values board coord)
          bv (block-values board coord)
          ]
      (set/difference all-values (set/union rv cv bv)))))

(defn filled? [board]
  (let [coll (range 0 9)
        all-values (set (map #(value-at board %) (coord-pairs coll)))]
    (not (contains? all-values 0))))

(defn rows [board]
  (let [row-coords (map #(list % 0) (range 0 9))]
    (map #(row-values board %) row-coords)))

(defn valid-rows? [board]
  (every? #(= 9 %) (map count (rows board))))

(defn cols [board]
  (let [col-coords (map #(list 0 %) (range 0 9))]
    (map #(col-values board %) col-coords)))

(defn valid-cols? [board]
  (every? #(= 9 %) (map count (cols board))))

(defn blocks [board]
  (let [block-coords (coord-pairs '(0 3 6))]
    (map #(block-values board %) block-coords)))

(defn valid-blocks? [board]
  (every? #(= 9 %) (map count (blocks board))))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [coords (coord-pairs (range 0 9))]
    (cond 
      (empty? coords) nil
      (not (has-value? board (first coords))) (first coords)
      :else (recur (rest coords)))))

(defn- solv-helper [board]
  (let [empty-point (find-empty-point board)]
    (if (nil? empty-point)
      (if (valid-solution? board) [board] [])
      (for [valid-value (valid-values-for board empty-point)
            solution (solv-helper (set-value-at board empty-point valid-value))]
        solution))
    )) 

(defn solve [board]
  (first (solv-helper board)))
