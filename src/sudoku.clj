(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

; (value-at sudoku-board [0 1])
(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (= (value-at board coord) 0)
    false
    true))

;(row-values sudoku-board [0 2]) ;=> #{0 5 3 7}
;(row-values sudoku-board [3 2]) ;=> #{0 8 6 3}

(defn unique-values [coll]
    (loop [items coll
           uniq-vals #{} ]
      (if (empty? items)
        uniq-vals
        (do 
          (if (contains? uniq-vals (first items))
            (recur (rest items) uniq-vals)
            (recur (rest items) (conj uniq-vals (first items) )))))))
  

(defn row-values [board coord]
  (let [ row (get board (first coord))]
    (unique-values row)))
    
;(col-values sudoku-board [4 8]) ;=> #{3 1 6 0 5 9}
(defn col-values [board coord]
  (let [columns (map #(get % (second coord)) board) ]
    (unique-values columns)))

;(coord-pairs [0 1])   ;=> [[0 0] [0 1]
                      ;    [1 0] [1 1]]
(defn coord-pairs [coords]
  (let [ pairs [] ]
    (for [ row coords
          column coords ]
      (conj pairs row column ))))

(defn coord->left-top [coord]
  (let [ helper (fn [num] 
                  (cond
                    (<= num 2) 0
                    (<= 3 num 5) 3
                    (<= 6 num 8) 6
                  :else -1 ))]
    [(helper (first coord))  (helper (second coord))]))

(defn get-block [ coord ]
  (for [ y (range (first coord) (+ (first coord) 3))
         x  (range (second coord) (+ (second coord) 3)) ]
    [y x]))

(defn block-values [board coord]
  (let [ left-top (coord->left-top coord)
         all-coords (get-block left-top) ]
    (unique-values (map #(value-at board %) all-coords))))
       

(defn valid-values-for [board coord]
  (if (= (value-at board coord) 0)
    (do (let [ block-vals (block-values board coord) 
               row-vals (row-values board coord) 
               col-vals (col-values board coord)
               taken-vals (clojure.set/union block-vals row-vals col-vals) ]
          (clojure.set/difference all-values taken-vals)))
    #{} ))
      

(defn filled? [board]
  (let [ is-filled? (fn [row] 
                      (empty? (filter true? (map #(= 0 %) row )))) ]
        (empty? (filter false? (map is-filled? board)))))

(defn rows [board]
  (for [ row (range 0 9) ]
    (row-values board [row 1])))
    
(defn valid-helper? [ lines ]
  (= (count (filter #(= (count %) 9) lines )) 9))
       
  
(defn valid-rows? [board]
  (valid-helper? (rows board)))
;  (let [ rows-status (for [ row (rows board)]
;                       (= (count row) 9 )) ]
;    (empty? (filter false? rows-status))))

(defn cols [board]
  (for [ column (range 0 9) ]
    (col-values board [0 column] )))

(defn valid-cols? [board]
  (valid-helper? (cols board)))

(defn blocks [board]
  (let [ block-coords [ [0 0] [0 3] [0 6]
                        [3 0] [3 3] [3 6]
                        [6 0] [6 3] [6 6] ]]
    (map #(block-values board %) block-coords)))

(defn valid-blocks? [board]
  (valid-helper? (blocks board)))

(defn valid-solution? [board]
  nil)

(defn set-value-at [board coord new-value]
  nil)

(defn find-empty-point [board]
  nil)

(defn solve [board]
  nil)
