(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (== 0 (value-at board coord))))

(defn row-values [board coord]
  (set (get board (get coord 0))))

(defn col-values [board coord]
  (let [y (get coord 1)]
    (set (reduce (fn [out a-seq] (cons (get a-seq y) out)) #{} board))))

(defn coord-pairs [coords]
  (for [rows coords
        cols coords]
    (conj [rows] cols)))

(defn block-values [board coord]
  (let [[x y] (map (fn [a] (* 3 (int (/ a 3)))) coord)
        coords (for [r (conj [x] (+ 1 x) (+ 2 x))
                      c (conj [y] (+ 1 y) (+ 2 y))]
                 (conj [r] c))]
    (set (reduce (fn [out c] (conj out (value-at board c))) [] coords)
    )))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (row-values board coord) (col-values board coord) (block-values board coord))))

(defn filled? [board]
  (let [coords (coord-pairs (range 9))]
    (not (.contains (reduce (fn [out coord] (conj out (value-at board coord))) [] coords) 0))))

(defn rows [board]
  (let [xcoords (range 9)]
    (reduce (fn [out x] (conj out (set (get board x)))) [] xcoords)))

(defn valid-rows? [board]
  (let [rows (rows board)]
    (reduce (fn [out row] (and out (empty? (clojure.set/difference all-values row)))) true rows)))

(defn cols [board]
  (let [ycoords (range 9)]
    (reduce (fn [out y] (conj out (col-values board (conj [0] y)))) [] ycoords)))

(defn valid-cols? [board]
  (let [cols (cols board)]
    (reduce (fn [out col] (and out (empty? (clojure.set/difference all-values col)))) true cols)))

(defn blocks [board]
  (let [pairs (coord-pairs [0 3 6])]
    (reduce (fn [out pair] (conj out (block-values board pair))) [] pairs)))

(defn valid-blocks? [board]
  (let [blocks (blocks board)]
    (reduce (fn [out block] (and out (empty? (clojure.set/difference all-values block)))) true blocks)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [pairs (coord-pairs (range 9))]
    (loop [pair pairs]
      (cond (empty? pair)
            nil
            (== 0 (value-at board (first pair)))
            (first pair)
            :else
            (recur (rest pair))))))

(defn solve-helper [solutions current-board]
  (let [empty-point (find-empty-point current-board)
        valid-values (valid-values-for current-board empty-point)
        current-solution (set-value-at current-board empty-point (first valid-values))]
    (cond
      (empty? valid-values)
      solutions
      (filled? current-solution)
      (conj solutions current-solution)
      :else
      (let [remaining valid-values]
        (for [elem remaining
              solution (solve-helper solutions (set-value-at current-board empty-point elem))]
          solution)))))

(defn solve [board]
  (first (solve-helper #{} board)))
