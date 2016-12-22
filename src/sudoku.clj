(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

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

(defn block-values [board coord]
  nil)

(defn valid-values-for [board coord]
  nil)

(defn filled? [board]
  nil)

(defn rows [board]
  nil)

(defn valid-rows? [board]
  nil)

(defn cols [board]
  nil)

(defn valid-cols? [board]
  nil)

(defn blocks [board]
  nil)

(defn valid-blocks? [board]
  nil)

(defn valid-solution? [board]
  nil)

(defn set-value-at [board coord new-value]
  nil)

(defn find-empty-point [board]
  nil)

(defn solve [board]
  nil)
