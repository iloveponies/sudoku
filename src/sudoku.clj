(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def sb
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

(def slvd
  (board [[5 3 4 6 7 8 9 1 2]
          [6 7 2 1 9 5 3 4 8]
          [1 9 8 3 4 2 5 6 7]
          [8 5 9 7 6 1 4 2 3]
          [4 2 6 8 5 3 7 9 1]
          [7 1 3 9 2 4 8 5 6]
          [9 6 1 5 3 7 2 8 4]
          [2 8 7 4 1 9 6 3 5]
          [3 4 5 2 8 6 1 7 9]]))

(def all-values (set (range 1 10)))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= (value-at board coord) 0)))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord
        fcn       (fn [acc row] (conj acc (get-in board [row col])))]
    (reduce fcn #{} (range 9))))


(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board coord]
  (let [block-fcn (fn [x] (* (int (/ x 3)) 3))
        [row col] coord
        brow      (block-fcn row)
        bcol      (block-fcn col)
        pairs     (for [r (range brow (+ brow 3))
                        c (range bcol (+ bcol 3))]
                    [r c])
        get-board (fn [pair] (value-at board pair))]
    (set (map get-board pairs))))

(defn valid-values-for [board coord]
  (if (not (= (value-at board coord) 0))
    #{}
    (let [frows  (set/difference all-values (row-values   board coord))
          fcols  (set/difference frows      (col-values   board coord))
          final  (set/difference fcols      (block-values board coord))]
      final)))

(defn filled? [board]
    (not (contains? (set (apply concat board) ) 0)))

(defn rows [board]
  (let [fcn (fn [acc row] (conj acc (row-values board [row row])))]
    (reduce fcn [] (range 9))))

(defn valid-rows? [board]
  (let [valid?  (fn [sett] (= sett all-values))
        row-seq (rows board)]
    (every? valid? row-seq)))

(defn cols [board]
  (let [fcn (fn [acc col] (conj acc (col-values board [col col])))]
    (reduce fcn [] (range 9))))

(defn valid-cols? [board]
  (let [valid?   (fn [sett] (= sett all-values))
        col-seq  (cols board)]
    (every? valid? col-seq)))

(defn blocks [board]
  (let [pairs (for [r [0 1 2]
                    c [0 1 2]]
                [(* r 3) (* c 3)])
        fcn   (fn [pair] (block-values board pair))]
    (map fcn pairs)))

(defn valid-blocks? [board]
  (let [valid?  (fn [sett] (= sett all-values))
        blk-seq (blocks board)]
    (every? valid? blk-seq)))

(defn valid-solution? [board]
  (and (valid-rows?   board)
       (valid-cols?   board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [pairs     (for [r (range 9)
                        c (range 9)] [r c])
        is-zero?   (fn [coord] (zero? (value-at board coord)))
        zero-pairs (filter is-zero? pairs)]
    (first zero-pairs)))

(defn options [board coord]
  (let [rv (row-values   board coord)
        cv (col-values   board coord)
        bv (block-values board coord)
        un (disj (set/union rv cv bv) 0)]
    (set/difference all-values un)))

(defn single-options [board]
  (let [pairs      (for [r (range 9)
                         c (range 9)] [r c])
        nonz?      (fn [pair] (not (= 0 (value-at board pair))))
        undec      (filter nonz? pairs)
        singleton? (fn [pair] (= 1 (count (options board pair))))
        singles    (filter singleton? undec)]
    singles))

(defn change-board [board singles]
  (if (empty? singles)
    board
    (let [coord    (first singles)
          value    (first (options board coord))
          new-board (set-value-at board coord value)]
      (change-board new-board (rest singles)))))


(defn solver [board]
  (if (valid-solution? board)
    board
    (let [empty-point (find-empty-point board)]
      (for [new-value (options board empty-point)
            solution  (solver (set-value-at board empty-point new-value))]
        solution))))

(defn solve [board]
  (apply vector (solver board)))

