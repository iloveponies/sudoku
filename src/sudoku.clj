(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [row _]]
  (set (get board row)))

(defn col-values [board [_ col]]
  (let [get-col (reduce (fn [vect-acc vect-in] (conj vect-acc (get vect-in col))) [] board)]
  (set get-col)))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    (vector row col)))

(defn block-values [board [row col]]
  (let [top-left-row (* 3 (int (/ row 3)))
        top-left-col (* 3 (int (/ col 3)))
        gen-coords (for [frow (range top-left-row (+ top-left-row 3))
                         fcol (range top-left-col (+ top-left-col 3))]
                     (vector frow fcol))]
    (reduce (fn [set-acc elem] (conj set-acc (value-at board elem))) #{} gen-coords)))

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn valid-values-for [board coord]
  (let [row-vals (row-values board coord)
        col-vals (col-values board coord)
        block-vals (block-values board coord)
        set-vals (set/union row-vals col-vals block-vals)]
    (if (zero? (value-at board coord))
      (set/difference all-values set-vals)
      #{})))

(defn filled? [board]
  (not (contains? (set (apply concat board)) 0)))


(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (= (repeat 9 all-values) (rows board)))

(defn cols [board]
  (let [col-coords (map (fn [elem] (vector 0 elem)) (range 0 9))]
    (map (fn [coord] (col-values board coord)) col-coords)))

(defn valid-cols? [board]
  (= (repeat 9 all-values) (cols board)))

(defn blocks [board]
  (map (fn [coord] (block-values board coord)) (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (= (repeat 9 all-values) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
   (assoc-in board coord new-value))

(defn find-first-zero [vector]
  (loop [coord 0
		     vect vector]
    (if (empty? vect)
      nil
      (if (zero? (first vect))
         coord
         (recur (inc coord) (rest vect))))))

(defn find-empty-point [board]
  (loop [x-coord 0
		     matrix board]
    (if (empty? matrix)
      nil
      (let [y-first-zero (find-first-zero (first matrix))]
      (if y-first-zero
        (vector x-coord y-first-zero)
        (recur (inc x-coord) (rest matrix)))))))

(defn solve-helper [first-zero board]
  (if (valid-solution? board)
    board
    (for [elem (valid-values-for board first-zero)
          solution
          (let [next-board (set-value-at board first-zero elem)
                next-zero (find-empty-point next-board)]
            (solve-helper next-zero next-board))]
        solution)))

(defn solve [board]
  (solve-helper (find-empty-point board) board))
