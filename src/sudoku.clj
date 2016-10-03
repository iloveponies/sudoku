(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord)
  )

(defn has-value? [board coord]
  (cond
   (= (value-at board coord) 0) false
   :else true
   )
  )

(defn row-values [board coord]
  (set (get board (get coord 0)))
  )

(defn col-values [board coord]
(set  (map (defn f [x] (get x (get coord 1))) board ) )
  )

(defn coord-pairs [coords]
  (for [x coords y coords]
    (vector x y)
    )
  )

(defn corner [coords]
  (let [[x y ] coords]
    (vector (- x (rem x 3)) (- y (rem y 3)))
    )
  )

(defn block-values [board coord]
  (let [[x y] (corner coord)]
    (set
     (vector
      (value-at board [(+ x 0)  (+ y 0) ])
      (value-at board [(+ x 0)  (+ y 1) ])
      (value-at board [(+ x 0)  (+ y 2) ])
      
      (value-at board [(+ x 1)  (+ y 0) ])
      (value-at board [(+ x 1)  (+ y 1) ])
      (value-at board [(+ x 1)  (+ y 2) ])
      
      (value-at board [(+ x 2)  (+ y 0) ])
      (value-at board [(+ x 2)  (+ y 1) ])
      (value-at board [(+ x 2)  (+ y 2) ])
     )
     )
    )
  )

(defn valid-values-for [board coord]
  (cond
   (not (= (value-at board coord) 0)) #{}
   :else
   (set/difference  #{1 2 3 4 5 6 7 8 9}
		    (set/union
		     (block-values board coord)
		     (row-values board coord)
		     (col-values board coord)
		     )
		    )
   )
  )

;;(defn row-values [row]  
;;  )

(defn filled? [board]
(not
 (contains?  (reduce  set/union
		      (map set board)
		      )
	     0)
 )
)

(defn rows [board]
  (map (fn f [x] (set x))  board)  
 )

(defn valid-rows? [board]
;;  (every? (map (defn f [x] (= 9 (count x))) board))
 (every? identity  (map (fn f [x] (and (not (contains? x 0)) (= 9 (count x)))) (rows board)))
  )

(defn cols [board]
  (for [x (range 0 9)] (col-values board [1 x]))
)

(defn valid-cols? [board]
 (every? identity  (map (fn f [x] (and (not (contains? x 0)) (= 9 (count x)))) (cols board)))
;;  board
)
(defn blocks [board]
;;  (coord-pairs [0 3 6])
(map (fn f [x] (block-values board x)) (coord-pairs [0 3 6]))

  )

(defn valid-blocks? [board]
 (every? identity  (map (fn f [x] (and (not (contains? x 0)) (= 9 (count x)))) (blocks board)))
  )

(defn valid-solution? [board]
  (and
   (valid-cols? board)
   (valid-rows? board)
   (valid-blocks? board)
   
   )
 )

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value)
  )

(defn find-empty-point [board]
  (first (filter (fn f [x] (= 0 (value-at board x ))) (coord-pairs (range 0 9))))
  
  )





;; (defn solve-rec [board]
;;   (cond
;;       (valid-solution? board) [board]
;;       (= nil (find-empty-point board)) '()
;;       :else
;;       (let [next-coord (find-empty-point board)]
;; 	(for  [new-val (valid-values-for  board next-coord)]
;; 	  (solve-rec (set-value-at board next-coord new-val ))
;; 	)
;;       )

;;       )
;;   )


(defn solve-rec [board]
  (cond
   
;;      (valid-solution? board) [board]
 ;;     (= nil (find-empty-point board)) '()
   ;;     :else

   (= nil (find-empty-point board))
   (if (valid-solution? board )  [board] '())
   :else 

   (let [next-coord (find-empty-point board)]
     (for  [new-val (valid-values-for  board next-coord)
	    solution (solve-rec (set-value-at board next-coord new-val ))]
       solution
       )
     )

   )
  )


(defn solve [board]
;;  board
 (first  (solve-rec board ))
)
