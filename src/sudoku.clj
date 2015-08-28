(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
      (get (get board (get coord 0)) (get coord 1)) 
)

(defn has-value? [board coord]
      (if (= 0 (value-at board coord)) false true) 
)

(defn row-values [board coord]
  (set (get board (get coord 0)))
)


(defn col-values [board coord]
  (set (map 
       (fn[x] (value-at board [x (get coord 1)])) 
        (range (count board))))
)

	

(defn coord-pairs [coords]
   (for [x coords y coords] [x y])
)

(defn block-values-helper [coord]
  (let [coord-counter (fn [x] (* 3 (int (/ (get coord x) 3))))
        give-range (fn [x] [x (+ x 1) (+ x 2)])
        x (coord-counter 0)
        y (coord-counter 1)]
    (vec (for [x-coords (give-range x)
               y-coords (give-range y)]
           (vector x-coords y-coords)))))


(defn value-at-board[board] (fn [coord] (value-at board coord)))

(defn block-values [board coord]
  (set (map (value-at-board board) (block-values-helper coord)))
)




(defn valid-values-for [board coord]
      (if (has-value? board coord) #{}
            (set/difference #{1 2 3 4 5 6 7 8 9} 
      	    (block-values board coord) 
	    (row-values board coord)
	    (col-values board coord)))
)


(defn filled? [board]
  (every? false? (reduce conj '[] (map (fn [row] (if (some #{0} row) true false)) board)))
)

(defn rows [board]
  (reduce conj '[] (map set board))
)

(defn transpose [m]
  (apply mapv vector m))
(defn cols [board]
  (rows (transpose board))
)

(defn valid-rows? [board]
      (if (every? true? (map (fn [row] (if (= #{1 2 3 4 5 6 7 8 9} (set row)) true false)) board)) true false)
)



(defn valid-cols? [board]
      (valid-rows? (transpose board))
)

(defn blocks [board]
      (let [centers [[0 0] [0 3] [0 6] [3 0] [3 3] [3 6] [6 0] [6 3] [6 6]]]
      (reduce conj [] (map (fn [d] (block-values board d)) centers))))

(defn valid-blocks? [board]
      (apply = (concat (blocks board) '[#{1 2 3 4 5 6 7 8 9}]))
)

(defn valid-solution? [board]
      (and 
      	   (valid-rows? board) 
	   (valid-cols? board) 
	   (valid-blocks? board))
)



(defn set-value-at [board coord new-value]
  (assoc board (get coord 0) 
  (assoc (get board (get coord 0)) 
  	 (get coord 1) new-value)))

(defn find-empty-point [board]
(first (filter #(zero? (value-at board %)) 
       (coord-pairs (range 0 9)) )))

(defn sum [a-seq]
  (reduce + a-seq))

(defn subset-sum-helper [a-set current-set target]
  (if (= (sum current-set) target)
    [current-set]
    (let [remaining (clojure.set/difference a-set current-set)]
      (for [elem remaining
            solution (subset-sum-helper a-set
                                        (conj current-set elem)
                                        target)]
        solution))))

(defn subset-sum [a-set target]
  (subset-sum-helper a-set #{} target))


(defn solve [board]
 (if (valid-solution? board) board
      (for [suggestion (valid-values-for board (find-empty-point board))
            solution (solve (set-value-at board (find-empty-point board) suggestion))]
            solution)))
