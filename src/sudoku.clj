(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get (get board (first coord)) (second coord)))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [row (get board (first coord))]
    (set row)))

(defn col-values [board coord]
  (loop [rows board
         col-set #{}]
    (if (nil? (first rows))
      col-set
      (recur (rest rows)(conj col-set (get (first rows) (second coord))) ))))

(defn coord-pairs [coords]
  (for [row coords
      col coords]
  (conj [] row col)))

(defn block-values [board coord]
  (let [index-helper (fn [x]
                       (cond
                        (< x 3) 0
                        (< x 6) 3
                        :else 6))
        y (index-helper (first coord))
        x (index-helper (second coord))
        block-helper (fn [board x y]
                      (for [coord (coord-pairs [0 1 2])]
                      (conj [] (+ y (first coord)) (+ x (second coord)))))]
    (set (for [c (block-helper board x y)]
           (value-at board c)))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference
     all-values
     (block-values board coord)
     (row-values board coord)
     (col-values board coord))))

(defn filled? [board]
  (let [every-coord (coord-pairs [0 1 2 3 4 5 6 7 8])]
    (every? (fn [coord] (has-value? board coord)) every-coord)))

(defn rows [board]
  (map (fn [coord] (row-values board coord)) (for [value all-values] (conj [] (dec value) 0))))

(defn valid-rows? [board]
  (every? (fn [x] (= x all-values)) (rows board)))

(defn cols [board]
  (map (fn [coord] (col-values board coord)) (for [value all-values] (conj [] 0 (dec value)))))

(defn valid-cols? [board]
  (every? (fn [x] (= x all-values)) (cols board)))

(defn blocks [board]
  (map (fn [coord] (block-values board coord)) (coord-pairs [2 4 6])))

(defn valid-blocks? [board]
  (every? (fn [x] (= x all-values)) (blocks board)))

(defn valid-solution? [board]
  (and (valid-blocks? board) (valid-rows? board) (valid-cols? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [every-coord (coord-pairs [0 1 2 3 4 5 6 7 8])]
    (if (filled? board)
      nil
      (loop [coords every-coord]
        (if (not (has-value? board (first coords)))
          (first coords)
          (recur (rest coords)))))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      [])
    (let [current (find-empty-point board)
          valids-for-current (valid-values-for board current)]
      (for [elem valids-for-current
            solution (solve (set-value-at board current elem))]
        solution))))
