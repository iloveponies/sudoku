(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(def aamulehti-five-stars
	(board [ [6 0 0 0 9 0 0 0 0]
                 [0 0 3 6 0 2 4 0 0]
                 [0 0 2 3 0 0 0 5 7]
                 [0 5 0 8 0 0 0 0 1]
                 [0 0 0 0 0 0 0 0 0]
                 [1 0 0 0 0 5 0 3 0]
                 [2 4 0 0 0 8 9 0 0]
                 [0 0 8 7 0 6 5 0 0]
                 [0 0 0 0 4 0 0 0 2] ]))

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
  (empty? (filter #(not (empty? (clojure.set/difference all-values %))) lines))) 
       
  
(defn valid-rows? [board]
  (valid-helper? (rows board)))

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
  (and (and (valid-rows? board) (valid-cols? board)) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (remove nil?
                 (for [ y (range 0 9)
                       x  (range 0 9) ]
                   (if (= (value-at board [y x]) 0)
                     [y x])))))
    
(defn solve-helper [curr-board]
  (if (valid-solution? curr-board)
    curr-board
    (let [ remaining (find-empty-point curr-board) 
           valid-values (valid-values-for curr-board remaining) ]
      (for [value valid-values 
            solution (solve-helper (set-value-at curr-board remaining value))]
        solution))))

(defn solve [board]
  (vec (solve-helper board)))

            

