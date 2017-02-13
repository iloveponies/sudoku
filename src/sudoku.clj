(ns sudoku
  (:require [clojure.set :as set]))

(def all-values #{1 2 3 4 5 6 7 8 9})

(def board identity)

(defn value-at [board coord]
  (let [[x y] coord]
  (get-in board [x y]))
)

(defn has-value? [board coord]
  (let [value (value-at board coord)]
    (if (zero? value) false true)
  )
)

(defn row-values [board coord]
  (let [[x y] coord]
  (set (get-in board [x])))
)

(defn col-values [board coord]
  (let [[x y] coord]
  (set (map (fn [i] (value-at board [i y])) (range 9))))
)

(defn coord-pairs [coords]
(vec (for [x coords
      y coords]
  (conj [] x y)
))
)

(defn yla-vasen [coord]
  (vector (* 3 (int (Math/floor (/ (first coord) 3))))
          (* 3 (int (Math/floor (/ (second coord) 3))))
  )
)


(defn block-values [board coord]
  (let [vasen (yla-vasen coord)]
  (set (for [i (range 3) k (range 3)]
     (let [eka (+ i (first vasen))
          toka (+ k (second vasen))]
     (value-at board [eka toka])))))
)

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
     (set/difference all-values
       (block-values board coord)
       (row-values board coord)
       (col-values board coord)
     )
  )
)

(defn filled? [board]
  (empty? (filter (fn [x] (contains? (set (get board x)) 0)) (range 9)))
)

(defn rows [board]
  (vec (map (fn [x] (row-values board [x 0])) (range 9)))
)

(defn valid-rows? [board]
 (every? (fn [row] (= row all-values)) (rows board))
)

(defn cols [board]
  (vec (map (fn [x] (col-values board [0 x])) (range 9)))
)

(defn valid-cols? [board]
  (every? (fn [col] (= col all-values)) (cols board))
)

(defn blocks [board]
   (vec (map (fn [coords] (block-values board coords)) (coord-pairs [0 3 6])))
)

(defn valid-blocks? [board]
  (every? (fn [block] (= block all-values)) (blocks board))
)

(defn valid-solution? [board]
  (if (and
      (valid-blocks? board)
      (valid-cols? board)
      (valid-rows? board)
      )
    true
    false
    )
)

(defn set-value-at [board coord new-value]
  (assoc-in board [(first coord) (second coord)] new-value)
)

(defn find-empty-point [board]
  nil)

(defn solve [board]
  nil)
