(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (= 0 (value-at board coord))
    false
    true))

(defn row-values [board coord]
  (let [[r1 r2 r3 r4 r5 r6 r7 r8 r9 :as rows] board]
    (set (get rows (get coord 0)))))

(defn col-values [board coord]
  (let [[r1 r2 r3 r4 r5 r6 r7 r8 r9 :as rows] board
        get-nth (fn get-nth [avec]
                  (nth avec (get coord 1)))]
    (set (map get-nth rows))))

(defn coord-pairs [coords]
  (for [row coords
        column coords]
    [row column]))

(defn block-values [board coord]
  (let [top-left (fn [[r c]]
                   [(* 3 (quot r 3)) (* 3 (quot c 3))])
        block-coord-pairs (fn [[r c]]
                            (for [row (range r (+ r 3))
                                  col (range c (+ c 3))]
                              [row col]))
        get-value (fn [valcoord]
                    (value-at board valcoord))
        coord-set (block-coord-pairs (top-left coord))]
    (set (map get-value coord-set))))

(defn valid-values-for [board coord]
  (let [used (clojure.set/union (row-values board coord)
                                (col-values board coord)
                                (block-values board coord))]
    (cond
      (= 0 (value-at board coord)) (clojure.set/difference all-values used)
      :else #{})))

(defn filled? [board]
  (let [board-vals (fn []
                     (for [row (range 0 8)
                           column (range 0 8)]
                       (value-at board [row column])))]
    (not (contains? (set (board-vals)) 0))))

(defn rows [board]
  (vec (for [row (range 0 9)]
          (row-values board [row 0]))))

(defn valid-rows? [board]
  (let [is-valid? (fn [values]
                    (if (empty? (clojure.set/difference all-values values))
                      true
                      false))]
    (not (contains? (frequencies (map is-valid? (rows board))) false))))

(defn cols [board]
  (vec (for [col (range 0 9)]
         (col-values board [0 col]))))

(defn valid-cols? [board]
  (let [is-valid? (fn [values]
                    (if (empty? (clojure.set/difference all-values values))
                      true
                      false))]
    (not (contains? (frequencies (map is-valid? (cols board))) false))))

(defn blocks [board]
  (vec (for [blrart (range 0 9 3)
             blcart (range 0 9 3)]
         (block-values board [blrart blcart]))))

(defn valid-blocks? [board]
  (let [is-valid? (fn [values]
                    (if (empty? (clojure.set/difference all-values values))
                      true
                      false))]
    (not (contains? (frequencies (map is-valid? (blocks board))) false))))

(defn valid-solution? [board]
  (and (valid-cols? board) (valid-rows? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [empties (fn []
                  (remove nil?
                          (for [row (range 0 9)
                                col (range 0 9)]
                            (if (= 0 (value-at board [row col]))
                              [row col]))))]
    (first (empties))))

(defn solve [board]
  (if (valid-solution? board)
    board
    (let [coords (find-empty-point board)
          valids (valid-values-for board coords)]
      (for [valid valids
            solution (solve (set-value-at board coords valid))]
        solution))))
