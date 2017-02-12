(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord)
)

(defn has-value? [board coord]
  (not (= (get-in board coord) 0))
)

(defn row-values [board coord]
   (set (get board (first coord)))
)

(defn col-values [board coord]
   (set (map (fn [x] (get x (second coord))) board ))
)

(defn coord-pairs [coords]
   (for [x coords
         y coords]
     (vector x y)
   )
)

(defn block-coord [coord]
   [(* (quot (first coord) 3) 3)
    (* (quot (second coord) 3) 3)]
)


(defn block-values [board coord]
   (let [[cx cy] (block-coord coord)]
      (set
        (for [x (range cx (+ cx 3))
              y (range cy (+ cy 3))
             ]
           (get-in board [x y])
        )
      )
   )
)
(defn valid-values-for [board coord]
   (if (not(= (value-at board coord) 0))
       #{}
       (reduce set/difference #{1 2 3 4 5 6 7 8 9}
               [(row-values board coord) (col-values board coord) (block-values board coord)]
       )
   )
)

(defn filled? [board]
   (not (contains? (set (flatten board)) 0))
)

(defn rows [board]
  (map set board)
)

(defn valid-rows? [board]
    (not (contains? (set (map (fn [row] (= row #{1 2 3 4 5 6 7 8 9})) (rows board))) false))
)



(defn cols [board]
   (into [] (for [y (range 0 9)]
               (set (map (fn [x] (get x y)) board ))
            )
   )
)

(defn valid-cols? [board]
  (not (contains? (set (map (fn [col] (= col #{1 2 3 4 5 6 7 8 9})) (cols board))) false))
)

(defn blocks [board]
   (for [cx [0 3 6]
         cy [0 3 6]
        ]
      (set
        (for [x (range cx (+ cx 3))
              y (range cy (+ cy 3))
             ]
           (get-in board [x y])
        )
      )
  )
)

(defn valid-blocks? [board]
  (not (contains? (set (map (fn [block] (= block #{1 2 3 4 5 6 7 8 9})) (blocks board))) false))
)

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board))
)

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value)
)

(defn find-empty-point [board]
    (first (for [x (range 0 9)
                 y (range 0 9)
                :when  (zero? (value-at board [x y]))]
             [x y]
           )
    )
)


(defn solve [board]
  (into [] (map (fn [x] (into [] x)) (partition 9 (flatten
    (if (filled? board)
        (if (valid-solution? board)
            board
            nil
        )
        (let [coord (find-empty-point board)]
            (for  [n  (valid-values-for board coord)]
               (solve ( set-value-at board coord n))
            )
        )
    )
  ))))
)

