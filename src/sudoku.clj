(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (nth (nth board (first coord)) (second coord)))

(defn has-value? [board coord]
  (if (not (= 0 (value-at board coord)))
    true
    false))

(defn row-values [board coord]
  (set (nth board (first coord))))

(defn col-values [board coord]
  (set 
    (for [x (range 0 9)]
    (value-at board [x (second coord)]))))

(defn coord-pairs [coords]
  (vec
  (for [x coords
        y coords]
    (apply vector [x y]))))

(defn block-coordinates [coord]
  (let [offset [(*(int (/ (first coord) 3)) 3) (*(int (/ (second coord) 3)) 3)]]
        (vec (map (fn [pos] (vec (map + pos offset))) (coord-pairs [0 1 2])))))

(defn block-values [board coord]
  (set
    (map (fn [pos] (value-at board pos)) (block-coordinates coord))))

(defn valid-values-for [board coord]
(if (has-value? board coord)
  #{}
  (set/difference all-values
                  (col-values board coord)
                  (row-values board coord)
                  (block-values board coord))))

(defn filled? [board]
  (not (some (fn [row] (contains? (set row) 0)) board)))

(defn rows [board]
  (vec (map set board)))

(defn valid-rows? [board]
  (every? (fn [row] (= all-values row)) (rows board)))

(defn cols [board]
  (for [x (range 0 9)] (col-values board [0 x])))

(defn valid-cols? [board]
  (every? (fn [col] (= all-values col)) (cols board)))

(defn blocks [board]
  (for [x (range 0 3)
        y (range 0 3)]
    (block-values board [(* 3 x) (* 3 y)])))

(defn valid-blocks? [board]
  (every? (fn [block] (= all-values block)) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (complement nil?)
                 (for [x (range 0 9)
                       y (range 0 9)]
                   (if (not (has-value? board [x y]))
                     [x y])))))

(defn solve [board]
  (if (valid-solution? board)
    board
    (let [ emptyCoord (find-empty-point board) ] 
      (for [step (valid-values-for board emptyCoord)
            nextStep (solve (set-value-at board emptyCoord step))]
        nextStep))))

