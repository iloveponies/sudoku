(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [row _]]
  (into #{} (get board row)))

(defn col-values [board [_ col]]
  (into #{} (for [row board] (get row col))))

(defn coord-pairs [coords]
  (vec
   (for [coord-outer coords
          coord-inner coords]
     (vector coord-outer coord-inner))))

(defn- -block-range [[x y]]
  (let [top-left-x (* (quot x 3) 3)
        top-left-y (* (quot y 3) 3)
        bottom-right-x (+ top-left-x 3)
        bottom-right-y (+ top-left-y 3)]
    [top-left-x top-left-y bottom-right-x bottom-right-y]))

(defn block-values [board coord]
  (let [[topleft-x topleft-y bottomright-x bottomright-y] (-block-range coord)]
    (into #{}
     (for [x (range topleft-x bottomright-x)
            y (range topleft-y bottomright-y)]
       (value-at board [x y])))))


(defn valid-values-for [board coord]
  (let [block-vals (block-values board coord)
        row-vals   (row-values board coord)
        col-vals   (col-values board coord)]
    (if (has-value? board coord)
      #{}
      (set/difference all-values row-vals col-vals block-vals))))

(defn- -board-values [board]
  (into #{}
   (for [row board
         value row]
     value)))

(defn filled? [board]
  (let [values (-board-values board)]
    (not (contains? values 0))))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? #(= all-values %) (rows board)))

(defn cols [board]
  (map set (apply map list board)))

(defn valid-cols? [board]
  (every? #(= all-values %) (cols board)))

(defn blocks [board]
  (let [num-cols (count (get board 0))
        num-rows (count board)]
    (for [x (range 0 num-cols 3)
          y (range 0 num-rows 3)]
      (block-values board [x y]))))

(defn valid-blocks? [board]
  (every? #(= all-values %) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter #(zero? (value-at board %)) (coord-pairs (range 0 (count board))))))

(defn solve [board]
  (vec
   (if (valid-solution? board)
      board
      (let [empty-point (find-empty-point board)
            valid-vals  (valid-values-for board empty-point)]
        (for [value valid-vals
              solution (solve (set-value-at board  empty-point value))]
          solution)))))


(def before-change
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))
(solve before-change)














