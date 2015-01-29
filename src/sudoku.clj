(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

;(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (= (value-at board coord) 0)
    false
    true))

(defn row-values [board [row _]]
  (set (get board row)))

(defn col-values [board [_ col]]
  (set (map (fn [row] (get row col)) board)))

(defn coord-pairs [coords]
    (for [x coords
          y coords]
      [x y]))

(defn get-top-corner [[row col]]
    [(* (int (/ row 3)) 3)
     (* (int (/ col 3)) 3)])


(defn block-values [board coords]
  (let [[top-left-row top-left-col]  (get-top-corner coords)
        index-values  (coord-pairs [0 1 2])
        values  (for [[row col] index-values]
                  (value-at board [(+ top-left-row row) (+ top-left-col col)]))]
    (set values)))

(defn valid-values-for [board coords]
  (if (has-value? board coords)
    #{}
    (let [row-vals (row-values board coords)
          col-vals (col-values board coords)
          block-vals (block-values board coords)]
      (set/difference (set (range 1 10)) (set/union row-vals col-vals block-vals)))))


(defn filled? [board]
  (not (contains? (set (apply concat board)) 0)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (let [rows  (rows board)
        helper  #(= %1 (set (range 1 10)))]
    (= (count (filter helper rows)) 9)))


(defn cols [board]
  (for [col (range 0 9)]
    (col-values board [0 col])))

(defn valid-cols? [board]
  (let [cols  (cols board)
        helper  #(= %1 (set (range 1 10)))]
    (= (count (filter helper cols)) 9)))

(defn blocks [board]
  (for [row [0 3 6]
        col [0 3 6]]
    (block-values board [row col])))

(defn valid-blocks? [board]
    (let [cols  (cols board)
        helper  #(= %1 (set (range 1 10)))]
    (= (count (filter helper cols)) 9)))

(defn valid-solution? [board]
  (and (valid-blocks? board) (valid-rows? board) (valid-cols? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [points (coord-pairs (range 9))]
    (cond
      (empty? points) nil
      (not (has-value? board (first points))) (first points)
      :else (recur (rest points)))))

(defn solver-helper [board]
  (if (valid-solution? board)
    [board]
    (let [coord (find-empty-point board)]
      (for [value (valid-values-for board coord)
            solution (solver-helper (set-value-at board coord value))]
        solution))))

(defn solve [board]
  (first (solver-helper board)))

;;backtacking sum example

(defn sum [a-seq]
  (reduce + a-seq))

(defn subset-sum-helper [a-set current-set target]
  (if (= (sum current-set) target)
    [current-set]
    (let [remaining (clojure.set/difference a-set current-set)]
      (for [elem remaining
            solution (subset-sum-helper a-set
                                        (conj current-set elem)
                                        target)]
        solution))))

(defn subset-sum [a-set target]
  (subset-sum-helper a-set #{} target))


