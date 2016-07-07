(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (== 0 (get-in board coord))))

(defn row-values [board coord]
  (let [[x y] coord]
    (set (get board x))))

(defn col-values [board coord]
  (let [[x y] coord]
    (set (map (fn [elem] (get elem y)) board))))

(defn coord-pairs [coords]
  (for [x  coords
        y  coords]
      (concat [x y] )))

(defn top-left [coords]
  (let [[x y] coords]
    [(- x (mod x 3)) (- y (mod y 3))])  
  )

(defn block-values [board coord]
  (set (let [[topx topy] (top-left coord)
             result '()]
    (set (map first(for [x (range topx (+ topx 3))
                         y (range topy (+ topy 3))]
                     (cons (get-in board [x y])  nil)))))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference 
      (set/difference 
        (set/difference all-values (block-values board coord))
        (row-values board coord))
      (col-values board coord))))

(defn filled? [board]
  (let [values 
        (apply set/union (for [x (range 0 9)]
                           (set/union (row-values board [x 0]))))
        ]
    (not (contains? values 0))))

(defn rows [board]
  (for [x (range 0 9)]
      (set/union (row-values board [x 0]))))

(defn valid? [a-set]
  (= a-set all-values))

(defn valid-rows? [board]
  (every? true? (map valid? (rows board))))

(defn cols [board]
   (for [x (range 0 9)]
      (set/union (col-values board [0 x]))))

(defn valid-cols? [board]
  (every? true? (map valid? (cols board))))

(defn blocks [board]
  (for [x [0 3 6]
        y [0 3 6]]
      (set/union (block-values board [x y]))))

(defn valid-blocks? [board]
  (every? true? (map valid? (blocks board))))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (disj (set (for [x (range 0 9)
                   y (range 0 9)]
               (if (not (has-value? board [x y]))
                 (concat [x y])))) nil)))

(defn sudoku-helper [board empty-set]
  (if (empty? empty-set)
    '()
	  (for [value (valid-values-for board (first empty-set))
	        target (set-value-at board (first empty-set) value)]
	    (if (valid-solution? target)
	      target
	      (sudoku-helper target (rest empty-set)))
	    ))
  )

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      [])
    (let [coord (find-empty-point board)]
      (for [value (valid-values-for board coord)
            target (solve (set-value-at board coord value))]
        target)))
  )
