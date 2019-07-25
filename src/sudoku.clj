(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= (value-at board coord) 0))

(defn row-values [board [row col]]
  (set (get board row)))

(defn col-values [board [row col]]
  (set (map #(get % col) board)))

(defn coord-pairs
  ([coords] (coord-pairs coords coords))
  ([rcoords ccoords]
   (for [row rcoords
         col ccoords]
     [row col])))

(defn block-values [board coord]
  (letfn [(top-left [[r c]]
                    [(- r (mod r 3)) (- c (mod c 3))])]
    (let [[tl-r tl-c] (top-left coord)]
      (set (map #(value-at board %)
                (coord-pairs (range tl-r (+ tl-r 3))
                             (range tl-c (+ tl-c 3))))))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [bv (block-values board coord)
          rv (row-values board coord)
          cv (col-values board coord)
          uv (clojure.set/union bv rv cv)]
      (clojure.set/difference all-values uv))))

(defn filled? [board]
  (let [all-coords (coord-pairs (range 9))]
    (every? #(has-value? board %) all-coords)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? #(= all-values %) (rows board)))

(defn cols [board]
  (map #(col-values board [% %]) (range 9)))

(defn valid-cols? [board]
  (every? #(= all-values %) (cols board)))

(defn blocks [board]
  (let [toplefts (coord-pairs [0 3 6])]
    (map #(block-values board %) toplefts)))

(defn valid-blocks? [board]
  (every? #(= all-values %) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [all-coords (coord-pairs (range 9))]
    (first (filter #(not (has-value? board %)) all-coords))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      (seq []))
    (let [e-lok (find-empty-point board)]
      (for [numb (valid-values-for board e-lok)
            solution (solve (set-value-at board e-lok numb))]
        solution))))
