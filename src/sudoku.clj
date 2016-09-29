(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board [row _]]
  (set (get board row)))

(defn col-values [board [_ col]]
  (set (map (fn [row] (get row col)) board)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board [x y]]
  (let [block-topleft-x (- x (mod x 3))
        block-topleft-y (- y (mod y 3))
        block-rows (range block-topleft-x (+ block-topleft-x 3))
        block-cols (range block-topleft-y (+ block-topleft-y 3))]
    (set
      (for [row block-rows
            col block-cols]
        (value-at board [row col])))))
      

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [used (clojure.set/union 
                 (row-values board coord)
                 (col-values board coord)
                 (block-values board coord))
          all (set (range 0 10))]
      (clojure.set/difference all used))))

(defn filled? [board]
  (not (some zero? (apply concat board))))

(defn rows [board]
  (map set board))

(defn valid? [a-set]
  (= a-set #{1 2 3 4 5 6 7 8 9}))
(defn valid-rows? [board]
  (not (some false? (map valid? (rows board)))))

(defn cols [board]
  (apply map (cons (fn [& xs] (set xs)) board)))

(defn valid-cols? [board]
  (not (some false? (map valid? (cols board)))))

(defn blocks [board]
  (for [topleft-x (range 0 9 3)
        topleft-y (range 0 9 3)]
    (set (for [x (range topleft-x (+ topleft-x 3))
               y (range topleft-y (+ topleft-y 3))]
           (value-at board [x y])))))

(defn valid-blocks? [board]
  (not (some false? (map valid? (blocks board)))))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [row 0
         col 0]
    (if (not (has-value? board [row col]))
      [row col]
      (if (= col 9)
        (recur (inc row) 0)
        (recur row (inc col))))))

(defn solve-helper [board]
 (if (filled? board)
   (if (valid-solution? board)
     [board]
     [])
   (let [coord (find-empty-point board)]
     (for [val (valid-values-for board coord)
           solution (solve-helper (set-value-at board coord val))]
       solution))))
  
(defn solve [board]
  (first (solve-helper board)))
