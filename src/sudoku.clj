(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)



(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (== 0 (value-at board coord))
    false true))


(defn row-values [board coord]
  (let [[rivi-coord _] coord
        rivi (get board rivi-coord)]
    (set rivi)))


(defn col-values [board coord]
  (let [[_ sar-coord] coord
        sarake (map (fn [rivi] (rivi sar-coord)) board)]
    (set sarake)))


(defn coord-pairs [coords]
  (for [rivi coords
        sarake coords]
    (list rivi sarake)))



(defn block-values-helper [coord]
  (let [[rivi sarake] coord
        block-rivi (* 3 (int (* 3 (/ rivi 9))))
        block-sarake (* 3 (int (* 3 (/ sarake 9))))]
    [block-rivi block-sarake]))

(defn block-values [board coord]
  (let [[top-rivi top-sarake] (block-values-helper coord)
        koordinaatit (map (fn[x] (map + x [top-rivi top-sarake])) (coord-pairs [0 1 2]))]
    (set (map (fn[x] (value-at board x)) koordinaatit))))

(defn valid-values-for [board coord]
  (if (== (value-at board coord) 0)
  (let [block-used (set (block-values board coord))
        col-used (set (col-values board coord))
        row-used (set (row-values board coord))
        all-used (set/union block-used col-used row-used)]
    (set/difference all-values all-used))
    #{}))



(defn filled? [board]
  (not (contains? (set (apply concat board)) 0)))


(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? (fn[x] (= x all-values)) (rows board)))

(defn cols [board]
  (for [col [0 1 2 3 4 5 6 7 8]]
    (set (col-values board [0 col]))))

(defn valid-cols? [board]
  (every? (fn[x] (= x all-values)) (cols board)))

(defn blocks [board]
  (for [row [0 1 2]
        col [0 1 2]]
    (set (block-values board [(* 3 row) (* 3 col)]))))


(defn valid-blocks? [board]
  (every? (fn[x] (= x all-values)) (cols board)))


(defn valid-solution? [board]
  (and (valid-blocks? board)
       (valid-cols? board)
       (valid-rows? board)))


(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))


(defn find-empty-point [board]
  (first (remove nil? (for [rivi (map dec all-values)
        sarake (map dec all-values)]
    (if (== 0 (value-at board [rivi sarake]))
      [rivi sarake])))))



(defn solve [board]
  nil)

