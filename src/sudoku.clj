(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

; Helper function to convert a collection to set (could also have used 'into')
(defn to-set [x] (apply conj '#{} x))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [r,_]]
  (to-set (get board r)))

(defn col-values [board [_,c]]
  (reduce (fn [x y] (conj x (get y c))) '#{} board))

; This function also takes in two sequences and returns pairs from them
(defn coord-pairs 
  ([coord-seq]  (for [x coord-seq
                      y coord-seq]
                  [x y]))
  ([coord-seq-a coord-seq-b] (for [x coord-seq-a
                                   y coord-seq-b]
                                [x y])))

; Helper function to return all the co-ordinates of a block give the co-ordinates
; of the top left corenr (origin) of the block
(defn block-coords [[origin-x origin-y]]
  (coord-pairs (range origin-x (+ origin-x 3)) (range origin-y (+ origin-y 3))))

; Helper function to return block values given the co-ordinates of top left
; corner (origin) of the block
(defn block-values-origin [board origin]
    (reduce (fn [x y] (conj x (value-at board y))) '#{} (block-coords origin)))

(defn block-values [board [r c]]
  (let [block-num (fn [x] (int (/ x 3.0)))
        origin [(* (block-num r) 3),(* (block-num c) 3)]]
        
    (block-values-origin board origin)))

(defn valid-values-for [board coord]
  (cond
   (has-value? board coord) '#{}
   :else (set/difference all-values (block-values board coord) (row-values board coord) (col-values board coord))))

(defn filled? [board]
  (let [row-full? (fn [x] (not (contains? (to-set x) 0)))]

    (reduce (fn [x y] (and x (row-full? y))) true board)))

(defn rows [board]
  (reduce (fn [x y] (conj x (to-set y))) [] board))

; Helper function to check validity of a sequence of values
(defn validity-helper [values]
  (reduce (fn [x y] (and x (empty? (set/difference all-values y)))) true values))

(defn valid-rows? [board]
  (validity-helper (rows board)))

(defn cols [board]
  (let [transpose (apply map vector board)]
    (rows transpose)))

(defn valid-cols? [board]
  (validity-helper (cols board)))

(defn blocks [board]
  (let [board-cols-count (count (first board))
        blocks-origin (coord-pairs (range 0 board-cols-count 3))]

    (reduce (fn [x origin] (conj x (block-values-origin board origin))) [] blocks-origin)))

(defn valid-blocks? [board]
  (validity-helper (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [board-cols-count (count (first board))]

    (loop [coords (coord-pairs (range 0 board-cols-count))]
      (let [point (first coords)]
        (cond
          (empty? coords) nil 
          (zero? (value-at board point)) point
          :else (recur (rest coords)))))))

(defn solver-helper [current-sol]
  (let [point (find-empty-point current-sol)]
    (cond
      (nil? point) (cond
                      (valid-solution? current-sol) current-sol
                      :else [])
      :else
        (let [valid-values (valid-values-for current-sol point)]
          (for [elem valid-values
                solution (solver-helper (set-value-at 
                                         current-sol point 
                                         elem))]
            solution)))))

(defn solve [board]
  (solver-helper board))







