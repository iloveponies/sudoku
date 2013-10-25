(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def allvalues #{0 1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (zero? (value-at board coord))
    false
    true))

(defn row-values [board coord]
  (let [[row _] coord]
    (reduce (fn [set x] (conj set x)) #{} (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (reduce
     (fn [set n] (conj set (value-at board [n col]))) #{} (range 9))))

(defn coord-pairs [coords]
  (for [value1 coords
        value2 coords]
    [value1 value2]))

(defn gener-block [topleft]
  (let [[row col] topleft]
    (for [r (range row (+ row 3))
          c (range col (+ col 3))]
      [r c])))

(defn topleft-coords [coord]
  (let [to3 (fn [n] (cond
                     (< n 3) 0
                     (< n 6) 3
                     :else 6))
        [row col] coord]
    [(to3 row) (to3 col)]))

(defn block-values [board coord]
  (reduce (fn [set vec] (conj set (value-at board vec)))
          #{} (gener-block (topleft-coords coord))))

(defn valid-values-for [board coord]
  (if (not (zero? (value-at board coord)))
    #{}
    (set/difference allvalues (set/union
                                 (row-values board coord)
                                 (col-values board coord)
                                 (block-values board coord)))))

(defn filled? [board]
  (let [allcoord (coord-pairs (range 0 9))
        taken? (fn [crds] (if (empty? (valid-values-for board crds))
                     true
                     false))]
    (loop [cur (first allcoord)
           bord (drop 1 allcoord)]
      (cond
       (empty? bord) true
       (not (taken? cur)) false
       :else (recur (first bord) (drop 1 bord))))))


(defn rows [board]
  (reduce (fn [vec n] (conj vec (row-values board [n 0]))) [] (range 0 9)))

(defn valid-rows? [board]
  (let [allvalid (set/difference allvalues #{0})]
    (loop [n 0]
      (cond
       (== n 9) true
       (not (= (get (rows board) n) allvalid)) false
       :else (recur (inc n))))))

(defn cols [board]
  (reduce (fn [vec n] (conj vec (col-values board [0 n]))) [] (range 0 9)))

(defn valid-cols? [board]
  (let [allvalid (set/difference allvalues #{0})]
    (loop [n 0]
      (cond
       (== n 9) true
       (not (= (get (cols board) n) allvalid)) false
       :else (recur (inc n))))))

(defn blocks [board]
  (reduce (fn [res cord] (conj res (block-values board cord)))
          [] (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (let [allvalid (set/difference allvalues #{0})]
    (loop [n 0]
      (cond
       (== n 9) true
       (not (= (get (blocks board) n) allvalid)) false
       :else (recur (inc n))))))

(defn valid-solution? [board]
  (reduce (fn [p pr] (and p (pr board)))
          true [valid-rows? valid-cols? valid-blocks?]))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [allcoord (vec (coord-pairs (range 9)))
        countcoords (count allcoord)
        taken? (fn [crds] (if (empty? (valid-values-for board crds))
                            true
                            false))]
    (loop [n 0]
      (cond
       (== n countcoords) nil
       (not (taken? (get allcoord n))) (get allcoord n)
       :else (recur (inc n))))))

(defn solve [board]
  nil)
