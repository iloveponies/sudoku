(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[_ col] coord
        get-col-value (fn [row] (get row col))]
    (set (map get-col-value board))))
    
(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn top-left-block-coordinates [board coord]
  (let [ [x y] coord
         tl (fn [n] (- n (mod n 3)))]
    [(tl x) (tl y)]))

(defn block-values [board coord]
  (let [tl (top-left-block-coordinates board coord)
        [tl-x tl-y] tl]
    (set (for [x (range tl-x (+ tl-x 3))
          y (range tl-y (+ tl-y 3))]
        (value-at board [x y])))))


        
(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
      (set/difference
       all-values
       (set/union (block-values board coord)
                  (row-values board coord)
                  (col-values board coord)))))

(defn filled? [board]
  (not-any? true? (map zero? (flatten board))))

(defn rows [board]
  (for [row board] (set row)))

(defn all-values-present? [s]
  (empty? (set/difference
           all-values
           s)))

(defn valid-rows? [board]
  (not-any? false? (map all-values-present? (rows board))))

(defn cols [board]
  (map (fn [colno](col-values board [0 colno])) 
       (range (count board))))

(defn valid-cols? [board]
    (not-any? false? (map all-values-present? (cols board))))

(defn blocks [board]
  (for [x (range 0 (count board) 3)
         y (range 0 (count board) 3)]
    (block-values board [x y])))

(defn valid-blocks? [board]
  (not-any? false? (map all-values-present? (blocks board))))

(defn valid-solution? [board]
  nil)

(defn set-value-at [board coord new-value]
  nil)

(defn find-empty-point [board]
  nil)

(defn solve [board]
  nil)
