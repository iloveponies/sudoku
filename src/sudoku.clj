(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (cond
   (= 0 (value-at board coord)) false
   :else true)) 
  

(defn row-values [board coord]
  (let [[row col] coord]
   (into #{} (get board row))))
  

(defn col-values [board coord]
  (let [[row col] coord
        transposed (vec(apply map vector board))]
   (into #{} (get transposed col))))
  

(defn coord-pairs [coords]
    (for [i coords
          j coords]
         [i j]))
     


(defn top-left-coords [coord]
 (let [[row col] coord
       y (* 3 (int (/ row 3)))
       x (* 3 (int (/ col 3)))]
      [y x]))

(defn block-values [board coord]
  (let [[topy topx] (top-left-coords coord)
        ycoords (range topy (+ topy 3))
        xcoords (range topx (+ topx 3))]
       (into #{} (for [y ycoords
                       x xcoords]
                  (value-at board [y x])))))
            

(defn valid-values-for [board coord]
  (let [block (block-values board coord)
        row   (row-values board coord)
        col   (col-values board coord)]
       (if (has-value? board coord)
        #{}
        (set/difference  all-values (set/union block row col)))))

(defn filled? [board]
  (not(contains? (into #{} (flatten board)) 0)))


(defn valid? [vals]
 (empty? (set/difference all-values vals)))

(defn rows [board]
  (into [] (for [row board]
            (into #{} row))))
  

(defn valid-rows? [board]
(cond
 (empty? board) true
 (not(valid? (first board))) false
  :else (valid-rows? (rest board))))

(defn cols [board]
  (let [transposed (vec(apply map vector board))]
   (into [] (for [col transposed]
             (into #{} col)))))

(defn valid-cols? [board]
 (let [cols (cols board)]
  (loop [c cols]
   (cond
    (empty? c) true
    (not(valid? (first c))) false
     :else (recur (rest c)))))) 
    

(defn blocks [board]
  (into [] (for [i (range 0 3)
                 j (range 0 3)]
            (block-values board [(* 3 i) (* 3 j)]))))

(defn valid-blocks? [board]
 (let [blocks (blocks board)]
   (loop [b blocks]
    (cond
     (empty? b) true
     (not(valid? (first b))) false
      :else (recur (rest b)))))) 
 

(defn valid-solution? [board]
  (and (valid-rows? board)
   (valid-cols? board)
   (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (into [] (for [i (range 0 9)
                        j (range 0 9)
                        :when (not(has-value? board [i j]))]
                   [i j]))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
     board
     [])
   (let [fempty (find-empty-point board)
         val (valid-values-for board fempty)]      
        (for [num val
              sol (solve (set-value-at board fempty num))]
             sol))))     
         
