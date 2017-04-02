(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [row _]]
  (set (get board row)))
; let might be a little prettier, but good coders get the idea
; that the last argument is a coordinate

(defn col-values [board [_ col]]
  (loop [acc #{}
         rows board]
    (if (empty? rows)
      acc
      (recur (conj acc (get (first rows) col)) (rest rows)))))

(defn coord-pairs [coords]
  (for [c1 coords 
        c2 coords]
    (vector c1 c2)))

(defn block-values [board coord]
  (let [top-left (for [c coord] (* 3 (int (/ c 3))))
        b-range (fn [n] (range n (+ 3 n)))
        coords (for [rows (b-range (first top-left))
                     cols (b-range (last top-left))]
                 (vector rows cols))]
    (loop [acc #{}
           cs coords]
      (if (empty? (first cs))
        acc
        (recur (conj acc (value-at board (first cs))) (rest cs))))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference
      all-values
      (set/union
        (row-values board coord)
        (col-values board coord)
        (block-values board coord)))))
        


(defn filled? [board]
  (loop [rows board]
    (if (empty? rows)
      true
      (if (not (empty? (filter zero? (first rows))))
        false  ; zero was found on a row
        (recur (rest rows))))))

(defn rows [board]
  (for [r board]
    (set r)))

(defn valid-rows? [board]
  (every?
    (fn [n] (== 9 n))
    (for [r (rows board)]
      (count r))))

(defn cols [board]
  (loop [acc []
         c 0]
    (if (== c 9)
      acc
      (recur (conj acc (set (col-values board [0 c]))) (inc c)))))

(defn valid-cols? [board]
  (every?
    (fn [n] (== 9 n))
    (for [c (cols board)]
      (count c))))

(defn blocks [board]
  (let [b-coords (coord-pairs [0 3 6])]
    (loop [acc []
           bc b-coords]
      (if (empty? bc)
        acc
        (recur (conj acc (set (block-values board (first bc))))
               (rest bc))))))

(defn valid-blocks? [board]
  (every?
    (fn [n] (== 9 n))
    (for [b (blocks board)]
      (count b))))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [r 0
         c 0]
    (if (not (has-value? board [r c]))
      [r c]
      (if (< 8 r)
        []
      (recur
        (if (== c 8) (inc r) r) (mod (inc c) 9))))))

(defn solve-helper [current-board]
  (if (filled? current-board)
    (if (valid-solution? current-board)
      current-board
      [])
    (let [loc (find-empty-point current-board)
          valid-set (valid-values-for current-board loc)]
      (for [elem valid-set
            solution (solve-helper
                       (set-value-at current-board loc elem))]
        solution))))

(defn solve [board]
  (solve-helper board))
  
