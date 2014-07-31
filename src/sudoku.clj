(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [[row _] coord]
    (into #{} (get board row))))

(defn col-values [board coord]
  (into #{} (map #(value-at board [% (last coord)]) (range 9))))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn up-cnr [coord]
  [(* 3 (int (/ (first coord) 3))) (* 3 (int (/ (last coord) 3)))])

(defn block-cords [coord]
  (map (fn [x] (map + (up-cnr coord) x)) (coord-pairs [0 1 2])))

(defn block-values [board coord]
  (into #{} (map #(value-at board %) (block-cords coord))))

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
    (set/difference all-values (set/union (row-values board coord)
                    (col-values board coord)
                    (block-values board coord)))))

(defn filled? [board]
  (every? #(has-value? board %) (coord-pairs [0 1 2 3 4 5 6 7 8])))

(defn rows [board]
  (map #(row-values board [% 0]) (range 9)))

(defn valid-rows? [board]
  (every? #(= all-values %) (rows board)))

(defn cols [board]
  (map #(col-values board [0 %]) (range 9)))

(defn valid-cols? [board]
  (every? #(= all-values %) (cols board)))

(defn blocks [board]
  (map #(block-values board %) (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (every? #(= all-values %) (blocks board)))

(defn valid-solution? [board]
  (and (valid-cols? board) (valid-rows? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [coord [(rand-int 8) (rand-int 8)]]
    (if (has-value? board coord)
      (find-empty-point board)
      coord)))

(defn solve-helper [a-board]
  (if (filled? a-board)
    (if (valid-solution? a-board)  [a-board] [])
    (let [next-pos (find-empty-point a-board)]
      (for [elem (valid-values-for a-board next-pos)
            solution (solve-helper (set-value-at a-board next-pos elem))]
        solution))))

(defn solve [my-board]
  (first (solve-helper my-board)))
