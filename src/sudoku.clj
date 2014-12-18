(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (zero? (value-at board coord)) false true))

(defn row-values [board coord]
  (let [[r c] coord]
    (loop [column 0
           values #{}]
      (if (>= column 9)
        values
        (recur (inc column) (conj values (value-at board [r column])))))))

(defn col-values [board coord]
  (let [[r c] coord]
    (loop [row 0
           values #{}]
      (if (>= row 9)
        values
        (recur (inc row) (conj values (value-at board [row c])))))))

(defn coord-pairs
  ([coords]
    (for [c1 coords
          c2 coords]
      (vector c1 c2)))
  ([row-coords col-coords]
    (for [r row-coords
          c col-coords]
      (vector r c))))

(defn block-top-left [coord]
  (let [[r c] coord]
    (vector (* (int (/ r 3)) 3) (* (int (/ c 3)) 3))))

(defn block-values [board coord]
  (let [[r c] (block-top-left coord)]
    (set (map (fn [x] (value-at board x))(coord-pairs (range r (+ r 3)) (range c (+ c 3)))))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [b board
          c coord]
      (set/difference (set/difference (set/difference all-values
                                                      (row-values b c))
                                      (col-values b c))
                      (block-values b c)))))

(defn rows [board]
  (let [r (range 0 9)
        row (fn [r-seq x] (conj r-seq (row-values board (vector x 0))))]
    (reduce row [] r)))

(defn cols [board]
  (let [c (range 0 9)
        col (fn [r-seq x] (conj r-seq (col-values board (vector 0 x))))]
    (reduce col [] c)))

(defn blocks [board]
  (let [coord-values (map (fn [x] (* x 3)) (range 0 3))
        block (fn [r-seq x] (conj r-seq (block-values board x)))]
    (reduce block [] (coord-pairs coord-values))))

(defn sel-filled [ret-value sel]
  (if ret-value
      (empty? (set/difference all-values sel))
      ret-value))

(defn valid-rows? [board]
  (reduce sel-filled true (rows board)))

(defn valid-cols? [board]
  (reduce sel-filled true (cols board)))

(defn valid-blocks? [board]
  (reduce sel-filled true (blocks board)))

(defn filled? [board]
  (valid-rows? board))

(defn valid-solution? [board]
  (and (valid-rows? board) (and (valid-cols? board) (valid-blocks? board))))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [check-coord (fn [crd] (if (has-value? board crd) [] crd))]
    (loop [crd []
           crd-pairs (coord-pairs [0 1 2 3 4 5 6 7 8])]
      (if (empty? crd)
        (recur (check-coord (first crd-pairs)) (rest crd-pairs))
        crd))))

(defn find-empty-points [board]
  (let [check-coord (fn [empty-points crd] (if (has-value? board crd)
                                             empty-points
                                             (conj empty-points crd)))]
    (reduce check-coord [] (coord-pairs [0 1 2 3 4 5 6 7 8]))))

(defn one-value-coords [board]
  (let [one-value (fn [crd] (= 1 (count (valid-values-for board crd))))]
    (filter one-value (find-empty-points board))))

(defn solve [board]
  (let [fill-values (fn [brd crd] (set-value-at brd crd (first (valid-values-for brd crd))))
        coords (one-value-coords board)]
    (if (empty? coords)
      board
      (solve (reduce fill-values board coords)))))
