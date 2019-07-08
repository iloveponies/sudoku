(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
 (not (zero? (get-in board coord))))

(defn row-values [board [row _]]
  (reduce conj #{} (get board row)))

(defn col-values [board [_ col]]
  (reduce #(conj %1 (nth %2 col)) #{} board))

(defn coord-pairs [coords]
  (for [row coords col coords]
    [row col]))

(defn block-origin [[row col]]
  [(- row (mod row 3)) (- col (mod col 3))])

(defn block-values [board coord]
  (let [[orow ocol] (block-origin coord)
        coords (for [row (range orow (+ orow 3)) 
                     col (range ocol (+ ocol 3))]
                 [row col])]
    (reduce #(conj %1 (get-in board %2)) #{} coords))) 


(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
    (set/difference all-values 
                    (set/union (row-values board coord)
                               (col-values board coord)
                               (block-values board coord)))))

(defn filled? [board]
  (every? true? (map #(has-value? board %) (coord-pairs (range 0 9)))))

(defn rows [board]
  (map #(row-values board [% 0]) (range 0 9)))

(defn valid-rows? [board]
  (every? true? (map #(= all-values %) (rows board))))

(defn cols [board]
  (map #(col-values board [0 %]) (range 0 9)))

(defn valid-cols? [board]
  (every? true? (map #(= all-values %) (cols board))))

(defn blocks [board]
  (let [coords (coord-pairs [0 3 6])]
    (map #(block-values board %) coords)))

(defn valid-blocks? [board]
  (every? true? (map #(= all-values %) (blocks board))))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [coords (coord-pairs (range 0 9))]
    (loop [s coords]
      (cond (empty? s) nil
            (zero? (value-at board (first s))) (first s)
            :else  (recur (rest s))))))

(defn solve-helper [board]
  (let [point (find-empty-point board)]
    (if (nil? point)
      (if (valid-solution? board) [board] [])
      (let [valid-vals (valid-values-for board point)]
        (for [v valid-vals
              solution (solve-helper (set-value-at board point v))]
          solution)))))

(defn solve [board]
  (first (solve-helper board)))
