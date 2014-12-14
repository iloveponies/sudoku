(ns sudoku
  (:require [clojure.set :as set]))

(def all-values #{1 2 3 4 5 6 7 8 9})

;(def block-start-coords [[0 0] [3 0] [6 0]
;                         [0 3] [3 3] [6 3]
;                         [0 6] [3 6] [6 6]])

(def block-start-coords [[0 0] [0 3] [0 6]
                         [3 0] [3 3] [3 6]
                         [6 0] [6 3] [6 6]])

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (= 0 (value-at board coord))
    false
    true))

(defn all-row-values [board coord]
  (let [[row col] coord]
    (get board row)))

(defn row-values [board coord]
  (set (all-row-values board coord)))

(defn all-col-values [board coord]
  (let [[dummy col] coord]
    (for [row (range 9)]
      (get-in board [row col]))))

(defn col-values [board coord]
  (set (all-col-values board coord)))

(defn coord-pairs [coords]
  (let [pairs []]
    (for [i coords
            j coords]
      (conj pairs i j))))

(defn block-start [coord]
  (* (int (/ coord 3)) 3))

(defn block-top-left [coords]
  (let [[row col] coords]
    [(block-start row) (block-start col)]))

(defn block-coords [top-left]
  (let [[row col] top-left
        rows (range row (+ row 3))
        cols (range col (+ col 3))
        pairs []]
    (for [i rows
            j cols]
      (conj pairs i j))))
    
(defn block-values [board coord]
  (let [coords (block-coords (block-top-left coord))]
    (reduce (fn [values coord] 
             (conj values (value-at board coord)))
        #{} coords)))
    ;(for [coord coords]
    ;  (value-at board coord))))
    
(defn valid-values-for [board coord]
  (if (= 0 (value-at board coord))
    (set/difference all-values 
                    (block-values board coord) 
                    (row-values board coord) 
                    (col-values board coord))
    #{}))

(defn blocks-filled [board]
  (for [block-start-coord block-start-coords]
    (empty? (set/difference all-values (block-values board block-start-coord)))))

(defn filled? [board]
  (every? identity (blocks-filled board)))

(defn rows [board]
  (for [row (range 9)]
    (set(row-values board [row 0]))))

(defn valid-element [values]
  (and (not (contains? values 0))(= 9 (count values))))

(defn valid-elements [elements]
  (for [element elements]
    (valid-element element)))

(defn valid-rows? [board]
  (every? identity (valid-elements(rows board))))

(defn cols [board]
  (for [col (range 9)]
    (set(col-values board [0 col]))))

(defn valid-cols? [board]
  (every? identity (valid-elements(cols board))))

(defn blocks [board]
  (for [block-start-coord block-start-coords]
    (set(block-values board block-start-coord))))

(defn valid-blocks? [board]
  (every? identity (valid-elements(blocks board))))

(defn valid-solution? [board]
  (every? identity [(valid-blocks? board) (valid-cols? board) (valid-rows? board)]))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [all-coords (coord-pairs [0 1 2 3 4 5 6 7 8])]
    (if (= 0 (value-at board (first all-coords)))
      (first all-coords)
      (recur (rest all-coords)))))

;check if you are at the end
; if so, is the solution valid?
;   if not, return an empty sequence
;   otherwise return [solution]
; if not
;   select an empty location
;   try solving with each valid value for that location

(defn solve-sudoku [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      [])
    (let [empty-point (find-empty-point board)
          values-for-point (valid-values-for board empty-point)]
      ;(println values-for-point)
      (for [value-for-point values-for-point
            solution (solve-sudoku (set-value-at board empty-point value-for-point))]
        solution))))

(defn solve [board]
  (loop [boards (solve-sudoku board)]
    (if (not (empty? (first boards)))
      (first boards)
      (recur (rest boards)))))
  
