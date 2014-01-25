(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (let [xCoord (second coord)]
    (reduce
     (fn [x y] (conj x (get y xCoord)))
     #{} board)))

(defn coord-pairs [coords]
  (for
    [coord1 coords
     coord2 coords]
    (vector coord1 coord2)))

(defn clamp-coord-topleft [coord]
  (let [[x y] coord]
    (vector (* 3(int (/ x 3))) (* 3(int (/ y 3))))))

(defn block-values [board coord]
  (let [[x y] (clamp-coord-topleft coord)]
    (set (for [curX (vector x (+ x 1) (+ x 2))
               curY (vector y (+ y 1) (+ y 2))]
           (value-at board (vector curX curY))))))

(defn valid-values-for [board coord]
  (cond
   (has-value? board coord) #{}
   :else (set/difference all-values (set/union (row-values board coord) (col-values board coord) (block-values board coord)))))

(defn filled? [board]
  (cond
   (empty? board) true
   (contains? (set (first board)) 0) false
   :else (recur (rest board))))

(defn rows [board]
  (loop [a-vec []
         i 0]
    (cond
     (= i 9) a-vec
     :else (recur
            (conj a-vec (set (row-values board (vector i 0))))
            (+ i 1)))))

(defn valid-rows? [board]
  (every? (fn [x] (empty? (set/difference all-values x))) (rows board)))

(defn cols [board]
  (loop [a-vec []
         i 0]
    (cond
     (= i 9) a-vec
     :else (recur
            (conj a-vec (set (col-values board (vector 0 i))))
            (+ i 1)))))

(defn valid-cols? [board]
  (every? (fn [x] (empty? (set/difference all-values x))) (cols board)))

(defn blocks [board]
    (for [x [0 3 6]
          y [0 3 6]]
      (block-values board (vector x y))))


(defn valid-blocks? [board]
  (every? (fn [x] (empty? (set/difference all-values x))) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [y 0]
    (cond
     (= y 9) nil
     (contains? (set (get board y)) 0) (vector y (.indexOf (get board y) 0))
     :else (recur (+ y 1)))))


(defn solve [board] nil)

(defn solve2 [board]
  (if (filled? board)
    (if (valid-solution? board) board nil)
    (let [coord (find-empty-point board)]
      (loop [remaining-values (valid-values-for board coord)]
          (let [solution (solve (set-value-at board coord (first remaining-values)))]
          (if (empty? solution) (recur (rest remaining-values)) nil))))))



