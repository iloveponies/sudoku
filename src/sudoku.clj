(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (zero? (value-at board coord)) false true))

(defn row-values [board coord]
  (into #{} (get board (first coord))))

(defn col-values [board coord]
  (let [[a b] coord]
    (into #{} (map (fn [v] (get v b)) board))))

(defn coord-pairs [coords]
  (for [i coords
        j coords]
    (vector i j)))


(defn block-values [board coord]
  (let [[x y] coord
        leftcorner (fn [z] (- z (mod z 3)))
        x (leftcorner x)
        y (leftcorner y)]
    (set (for [row (range x (+ x 3))
               col (range y (+ y 3))]
          (value-at board [row col])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference (set (range 1 10))
                    (row-values board coord)
                    (col-values board coord)
                    (block-values board coord))))

(defn column-not-empty? [coord]
  (let [[x y] coord]
    (if (empty? (get board x)) true false)))


(defn filled? [board]
  (every? (fn [row] (if (or (empty? row) (every? (fn [x] (> x 0)) row)) true false )) board))

(defn rows [board]
  (for [row (range 0 9)]
    (row-values board [row 0])))

(defn valid-rows? [board]
  (every?
   (fn [row]
     (and
      (empty? (set/difference
              (set row)
              (set (range 1 10))))
      (apply distinct? row))) board))

(defn cols [board]
  (for [col (range 0 9)]
    (col-values board [0 col])))

(defn valid-cols? [board]
 (every? (fn [x] (= x (set (range 1 10)))) (cols board)))

(defn blocks [board]
   (for [r #{0 3 6}
         c #{0 3 6}]
     (block-values board [r c])))

(defn valid-blocks? [board]
   (every? (fn [x] (= x (set (range 1 10)))) (blocks board)))

(defn valid-solution? [board]
  (and (valid-blocks? board) (valid-cols? board) (valid-rows? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [has-zero? (fn [coord] (not (has-value? board coord)))]
    (first (filter has-zero?
                   (coord-pairs (range (count board)))))))

(defn solve [board]
  (if (and (filled? board)
           (valid-solution? board))
      board
    (let [coord (find-empty-point board)]
      (if (nil? coord)
          board
          (first (filter boolean
                         (for [new-value (valid-values-for board coord)]
                              (solve (set-value-at board coord new-value)))))))))
