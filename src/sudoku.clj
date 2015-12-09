(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (get-in board coord))))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (set (for [x (range 0  (count board))] 
           (get-in board (vector x (second coord))))))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    (vector x y)))

(defn tl_coord [[x y]]
	 (let [tl-x (* (quot x 3) 3)
	  tl-y (* (quot y 3) 3)]
	 [tl-x tl-y])
  )

(defn block-values [board coord]
  (let [x (first (tl_coord coord))
        y (second (tl_coord coord))]
     (set (for [x (range x (+ x 3)) 
               y (range y (+ y 3))] 
             (get-in board [x y])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
      (set/difference all-values (set/union (block-values board coord)
                                            (row-values board coord)
                                            (col-values board coord)))))

(defn filled? [board]
  (every? true? (map (fn [x] (not (contains? x 0))) (map set board))))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? true?  (map (fn [x] (= all-values x)) (rows board))))

(defn cols [board]
  (for [idx (range 0 (count board))] 
       (col-values board [0 idx])))

(defn valid-cols? [board]
  (every? true?  (map (fn [x] (= all-values x)) (cols board))))

(defn blocks [board]
  (for [x [0 3 6]
        y [0 3 6]]
     (block-values board [x y])))

(defn valid-blocks? [board]
  (every? true?  (map (fn [x] (= all-values x)) (blocks board))))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (fn [coord] (= 0 (value-at board coord)))
    (for [x (range 0 9)
          y (range 0 9)]
         (vector x y)))))

(defn solve [board]
  (let [emptyPs (find-empty-point board)]
     (if (empty? emptyPs) 
        (if (valid-solution? board) board []) 
        (let [ppp (valid-values-for board emptyPs)] 
            (for [p ppp
                  res (solve (set-value-at board emptyPs p))]
               res)))))
