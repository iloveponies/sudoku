(ns sudoku
  (:require [clojure.set :as set]))

(def all-values #{1 2 3 4 5 6 7 8 9})

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (= (value-at board coord) 0)
      false
      true))

(defn row-values [board coord]
  (set (get board (get coord 0))))

(defn col-values [board coord]
  (loop [acc #{}
         number 0]
    (if (empty? (get board number))
        acc
        (recur (conj acc (get-in board [number (get coord 1)])) (inc number)))))

(defn coord-pairs [coords]
  (vec (for [coord1 coords
        coord2 coords]
    (conj [coord1] coord2))))

(defn coords-of-block [coord]
  (let [coord-counter (fn [x] (* 3 (int (/ (get coord x) 3))))
        give-range (fn [x] [x (+ x 1) (+ x 2)])
        x (coord-counter 0)
        y (coord-counter 1)]
    (vec (for [x-coords (give-range x)
               y-coords (give-range y)]
           (vector x-coords y-coords)))))

(defn block-values [board coord]
  (set (map (fn [x] (value-at board x)) (coords-of-block coord))))


(defn valid-values-for [board coord]
  (let [numbers-in-block (block-values board coord)
        numbers-in-row (row-values board coord)
        numbers-in-col (col-values board coord)
        valid-numbers (clojure.set/difference all-values
                                              numbers-in-block
                                              numbers-in-row
                                              numbers-in-col)]
  (if (= 0 (value-at board coord))
    valid-numbers
    #{})))

(def index-range (vec (range 9)))
(def all-rows (for [x index-range]  [x 0]))
(def all-columns (for [x index-range]  [0 x]))
(def all-blocks (for [x [0 3 6]
                      y [0 3 6]]
                  [x y]))

(defn filled? [board]
  (let [all-values-on-board (reduce (fn [x y] (clojure.set/union x (row-values board y))) #{} all-rows)]
    (not (contains? all-values-on-board 0))))

(defn only-valid? [x y]
  (and x (= all-values y)))

(defn rows [board]
  (reduce (fn [x y] (conj x (row-values board y))) [] all-rows))

(defn valid-rows? [board]
  (reduce only-valid? true (rows board)))

(defn cols [board]
  (reduce (fn [x y] (conj x (col-values board y))) [] all-columns))

(defn valid-cols? [board]
  (reduce only-valid? true (cols board)))

(defn blocks [board]
  (reduce (fn [x y] (conj x (block-values board y))) [] all-blocks))

(defn valid-blocks? [board]
  (reduce only-valid? true (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [x 0
         y 0]
    (cond (= y 9) nil
          (= x 9) (recur 0 (inc y))
          (= 0 (value-at board (vector x y))) (vector x y)
          :else (recur (inc x) y))))

(defn solve [board-now]
  (if (= nil (find-empty-point board-now))
      (if (valid-solution? board-now) ;ei tyhjiä
          board-now
          [])
      (let [coord (find-empty-point board-now)]
        (for [allowed-values (vec (valid-values-for board-now coord))
              set-value (solve (set-value-at board-now coord allowed-values))]
          set-value))))
