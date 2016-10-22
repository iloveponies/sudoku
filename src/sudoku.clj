(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(def indices [0 1 2 3 4 5 6 7 8])

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (= (value-at board coord) 0)
    false
    true))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (set (for [number indices]
         (value-at board [number (second coord)]))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
     [row col]))

(defn block-values [board coord]
  (let [top-left (fn [c]
                   [(- (first c) (mod (first c) 3))
                    (- (second c) (mod (second c) 3))])
        coord-seq (fn [init] [init (inc init) (+ init 2)])]
    (set (for [row (coord-seq (first (top-left coord)))
               col (coord-seq (second (top-left coord)))]
           (value-at board [row col])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (set/union
                                 (row-values board coord)
                                 (col-values board coord)
                                 (block-values board coord)))))

(defn filled? [board]
  (let [all-numbers (fn [] (for [pair (coord-pairs indices)]
                             (value-at board pair)))]
    (not (contains? (set (all-numbers)) 0))))

(defn rows [board]
  (for [number indices]
    (row-values board [number 0])))

(defn valid-rows? [board]
  (let [a-set (fn [] (set (for [row (rows board)]
                                (= all-values row))))]
    (not (contains? (a-set) false))))

(defn cols [board]
  (for [number indices]
    (col-values board [0 number])))

(defn valid-cols? [board]
  (let [a-set (fn [] (set (for [col (cols board)]
                                (= all-values col))))]
    (not (contains? (a-set) false))))

(defn blocks [board]
  (for [pair [[0 0] [0 3] [0 6]
              [3 0] [3 3] [3 6]
              [6 0] [6 3] [6 6]]]
    (block-values board pair)))

(defn valid-blocks? [board]
  (let [a-set (fn [] (set (for [block (blocks board)]
                                (= all-values block))))]
    (not (contains? (a-set) false))))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [coordinates (fn [] (mapv identity (coord-pairs indices)))
        pred (fn [co] (not (zero? (value-at board co))))]
    (if (empty? (drop-while pred (coordinates)))
      nil
      (first (drop-while pred (coordinates))))))

(defn find-a-solution [a-board solutions]
  (if (filled? a-board)
    [a-board]
    (for [valid-vals (valid-values-for a-board (find-empty-point a-board))
          a-solution (find-a-solution (set-value-at a-board (find-empty-point a-board) valid-vals) solutions)]
      a-solution)))

(defn solve [board]
  (first (find-a-solution board [])))
