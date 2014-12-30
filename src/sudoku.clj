(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (set (board (coord 0))))

(defn col-values [board coord]
  (reduce
    (fn [a-set e]
      (conj a-set (e (coord 1))))
    #{}
    board))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board coord]
  (let [offset (for [x coord] (- x (mod x 3)))
        tl-block (set (coord-pairs [0 1 2]))]
    (set (map (partial value-at board) 
              (for [block-coord tl-block]
               (map + block-coord offset))))))
      
(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference 
      all-values
      (set/union (row-values board coord)
                 (col-values board coord)
                 (block-values board coord)))))

(defn filled? [board]
  (not (contains?
         (set (reduce concat board)) 0)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (loop [rs (rows board)]
    (if (empty? rs)
      true
      (if (= all-values (first rs))
        (recur (rest rs))
        false))))

(defn cols [board]
  (for [col (range 0 9)]
    (set (col-values board [0 col]))))

(defn valid-cols? [board]
  (loop [cs (cols board)]
    (if (empty? cs)
      true
      (if (= all-values (first cs))
        (recur (rest cs))
        false))))

(defn blocks [board]
  (for [coord (coord-pairs [0 3 6])]
    (set (block-values board coord))))

(defn valid-blocks? [board]
  (loop [bs (blocks board)]
    (if (empty? bs)
      true
      (if (= all-values (first bs))
        (recur (rest bs))
        false))))

(defn valid-solution? [board]
  (and 
    (valid-rows? board)
    (valid-cols? board)
    (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [coords (coord-pairs (range 0 9))]
    (cond
      (empty? coords) nil
      (has-value? board (first coords)) (recur (rest coords))
      :else (first coords))))

(defn solve [board] 
  (first (if (filled? board)
           (if (valid-solution? board)
             [board]    
             ())
           (let [empty-coord (find-empty-point board)]
             (reduce
               (fn [coll value]
                 (let [sol (solve (set-value-at board empty-coord value))]
                   (if (empty? sol)
                     coll
                     (conj coll sol))))
               [] (valid-values-for board empty-coord))))))
  
  
  