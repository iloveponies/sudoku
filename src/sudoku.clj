(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= (value-at board coord) 0)))

(defn row-values [board coord]
  (into #{} (get board (first coord))))

(defn col-values [board coord]
  (loop [values #{}
          i 0]
     (if (= i 9)
       values
       (recur
         (conj
           values
           (value-at board [i (last coord)]))
         (inc i)))))

(defn coord-pairs [coords]
  (for [c1 coords
         c2 coords]
     [c1 c2]))
(defn top-left [coord]
   (let [r (first coord)
         c (last coord)]
     (cond
      (< r 3) (cond
                 (< c 3) [0 0]
                 (< c 6) [0 3]
                 (< c 9) [0 6])
      (< r 6) (cond
                 (< c 3) [3 0]
                 (< c 6) [3 3]
                 (< c 9) [3 6])
       (< r 9) (cond
                 (< c 3) [6 0]
                 (< c 6) [6 3]
                 (< c 9) [6 6]))))
 
 (top-left [8 8])


(defn block-values [board coord]
   (let [x (first (top-left coord))
         y (last (top-left coord))]
     (into #{}
           (concat
             [(value-at board [x y])]
             [(value-at board [x (+ 1 y)])]
             [(value-at board [x (+ 2 y)])]
             [(value-at board [(+ 1 x) y])]
             [(value-at board [(+ 1 x) (+ 1 y)])]
             [(value-at board [(+ 1 x) (+ 2 y)])]
             [(value-at board [(+ 2 x) y])]
             [(value-at board [(+ 2 x) (+ 1 y)])]
             [(value-at board [(+ 2 x) (+ 2 y)])]))))


(defn valid-values-for [board coord]
  (cond
     (has-value? board coord) #{}
     :else (let [used (set/union
                        (block-values board coord)
                        (row-values board coord)
                        (col-values board coord))]
             (set/difference all-values used))))
 
 (defn board-to-seq [board]
   (reduce (fn [acc y] (concat acc y)) [] board))
 

(defn filled? [board]
  (every? (fn [x] (has-value? board x)) (coord-pairs (range 9))))
 
 (def rownums [0 1 2 3 4 5 6 7 8])

(defn rows [board]
  (reduce (fn [acc row] (conj acc (row-values board [row 0]))) [] rownums))
 
 
 (defn valid-line? [line]
   (empty? (set/difference all-values line)))

(defn valid-rows? [board]
  (every? (fn [line] (valid-line? line)) (rows board)))

(defn cols [board]
  (reduce (fn [acc col] (conj acc (col-values board [0 col]))) [] rownums))

(defn valid-cols? [board]
  (every? (fn [col] (valid-line? col)) (cols board)))

(defn blocks [board]
  (reduce (fn [acc coord] (conj acc (block-values board coord))) [] (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (every? (fn [block] (valid-line? block)) (blocks board)))
 
 (coord-pairs [0 3 6])

(defn valid-solution? [board]
  (and
     (valid-rows? board)
     (valid-cols? board)
     (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [coords (coord-pairs (range 9))]
     (first (filter (fn [x] (not (has-value? board x))) coords))))
 
 
 (defn solve-helper [curr]
   (if (filled? curr)
     (if (valid-solution? curr)
       [curr]
       [])
     (let [insert (find-empty-point curr)
           valid-values (valid-values-for curr insert)]
       (for [value valid-values
             solution (solve-helper (set-value-at curr insert value))]
         solution))))

(defn solve [board]
  (first (solve-helper board)))
 
 (def tb (board
          [[5 3 4 6 7 8 9 1 2]
           [6 7 2 1 9 5 3 4 8]
           [1 9 8 3 4 2 5 6 7]
           [8 5 9 7 6 1 0 2 3]
           [4 2 6 8 5 3 7 9 1]
           [7 1 3 0 2 0 8 5 6]
           [9 6 1 5 0 7 2 8 4]
           [2 8 7 4 1 0 0 3 5]
           [3 4 5 2 8 0 0 7 9]]))
 
 (def tc [5 5])
 (top-left tc)
 (block-values tb (top-left tc))
(row-values tb tc)
 (col-values tb tc)
 (valid-values-for tb tc)
 (solve tb)
