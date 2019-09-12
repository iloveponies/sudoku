(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board [y x]]
  (set (get board y)))

(defn col-values [board [y x]]
  (set (for [row board] (get row x))))

(defn coord-pairs [coords]
  (for [x coords y coords] [x y]))

(defn block-start [[x y]]
  [(* 3 (quot x 3))
   (* 3 (quot y 3))])

(defn block-values [board coord]
  (set
  (let [[y x] (block-start coord)]
    (for [row (range y (+ y 3))
          column (range x (+ x 3))]
      (value-at board [row column])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
    (set/difference all-values
                    (row-values board coord)
                    (col-values board coord)
                    (block-values board coord))))
(def all-coords
  (for [y (range 9)
         x (range 9)]
     [y x]))

(defn filled? [board]
  (every? #(has-value? board %)
   all-coords))

(defn rows [board]
  (for [row board]
    (set row)))

(defn valid-rows? [board]
  (every? #(= all-values %)
          (rows board)))

(defn cols [board]
  (for [x (range 9)]
    (col-values board [0 x])))

(defn valid-cols? [board]
  (every? #(= all-values %)
          (cols board)))

(defn blocks [board]
  (for [x [0 3 6]
       y [0 3 6]]
    (block-values board [x y])))

(defn valid-blocks? [board]
  (every? #(= all-values %)
          (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board [y x] new-value]
  (assoc board y
    (assoc (get board y) x new-value)))

(defn find-empty-point [board]
  (first (filter #(= 0 (value-at board %))
                 all-coords)))


(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      nil)
    (let [newcoord (find-empty-point board)]
    (first
     (filter
      #(not (nil? %))
      (for [x (valid-values-for board newcoord)]
        (solve (set-value-at board
                             newcoord
                             x))))))))




