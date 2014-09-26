(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
 (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [r c]]
  (set (get board r)))

(defn col-values [board [r c]]
  (set (map (fn [row] (get row c)) board)))

(defn coord-pairs [coords]
  (vec (for [x coords
             y coords]
         [x y]
         )))
     
(defn block-topleft [[r c]]
  [(* 3 (int (/ r 3))) (* 3 (int (/ c 3)))])

(defn block-values [board [r c]]
 (let [[top left] (block-topleft [r c])
       values (for [row (range top (+ top 3))
                    col (range left (+ left 3))]
                (value-at board [row col]))]
   (set values)))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values 
                    (set/union (block-values board coord) 
                               (row-values board coord) 
                               (col-values board coord)))))

(defn filled? [board]
  (every? (fn [coord] (has-value? board coord)) (coord-pairs (range 0 9))))

(defn rows [board]
  (vec (map (fn [row] (set row)) board)))

(defn valid-rows? [board]
  (every? (fn [row] (= all-values (row-values board [row 0]))) (range 0 9)))

(defn cols [board]
  (vec (for [col (range 0 9)]
         (set (map (fn [row] (get row col)) board)))))
         
(defn valid-cols? [board]
  (every? (fn [col] (= all-values (col-values board [0 col]))) (range 0 9)))

(defn blocks [board]
  (vec (for [col [0 3 6]
             row [0 3 6]]
         (block-values board [col row]))))

(defn valid-blocks? [board]
  (let [toplefts (for [row [0 3 6] 
                       col [0 3 6]]
                   [row col])]
  (every? (fn [coord] (= all-values (block-values board coord))) toplefts)))

(defn valid-solution? [board]
  (and (valid-cols? board) (valid-rows? board) (valid-blocks? board)))
  
(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [coords (for [row (range 0 9) 
                     col (range 0 9)]
                   [row col])]
    (some (fn [coord] (if (has-value? board coord)
                        false
                        coord)) coords))) 

(defn solve [board]
  (let [empty-point (find-empty-point board)]
    (if (not empty-point)
      (if (valid-solution? board)
        board
        [])
      (some not-empty (for [value (valid-values-for board empty-point)]
                           (solve (set-value-at board empty-point value)))))))

        
