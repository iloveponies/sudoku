(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (< 0 (value-at board coord)))

(defn row-values [board coord]
  (let [[y _] coord
        row (get board y)]
    (set row)))

(defn col-values [board coord]
  (loop [values #{}
         index 0
         y (get coord 1)]
    (if
      (= index 9)
      values
      (recur (conj values (value-at board [index y])) (inc index) y) )))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    (conj [] x y)))

(defn block-helper [coords]
  (let [[x y] coords]
    (for [i [x (+ x 1) (+ x 2)]
          j [y (+ y 1) (+ y 2)]]
      (conj [] i j))))

(defn block-values [board coord]
  (let [[x y] coord
        value (fn [x] (value-at board x))]
    (set (map value (block-helper [(* 3 (quot x 3)) (* 3 (quot y 3))])))))

(defn valid-values-for [board coord]
  (if
    (has-value? board coord)
    #{}
    (set/difference all-values (set/union (row-values board coord) (col-values board coord) (block-values board coord)))))

(defn filled? [board]
  (loop [coords (coord-pairs (subvec (vec all-values) 0 8))
         b board]
    (cond
     (empty? coords) true
     (has-value? b (first coords)) (recur (rest coords) b)
     :else false)))

(defn valid? [s]
  (= s all-values))

(defn rows [board]
  (loop [b board
         index 0
         rws []]
    (if
      (= index 9)
      rws
      (recur b (inc index) (conj rws (row-values b [index 0]))))))

(defn valid-rows? [board]
  (loop [sets (rows board)]
    (cond
     (empty? sets) true
     (valid? (first sets)) (recur (rest sets))
     :else false
      )))

(defn cols [board]
   (loop [b board
         index 0
         cls []]
    (if
      (= index 9)
      cls
      (recur b (inc index) (conj cls (col-values b [0 index]))))))

(defn valid-cols? [board]
  (loop [sets (cols board)]
    (cond
     (empty? sets) true
     (valid? (first sets)) (recur (rest sets))
     :else false
      )))

(defn blocks [board]
  (let [h-block-values (fn [x] (block-values board x))]
    (map h-block-values (coord-pairs [0 3 6]))))

(defn valid-blocks? [board]
  (loop [sets (blocks board)]
    (cond
     (empty? sets) true
     (valid? (first sets)) (recur (rest sets))
     :else false
      )))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [b board
         i 0
         j 0]
    (cond
     (not (has-value? b [i j])) [i j]
     (< i 8) (recur b (inc i) j)
     (< j 8) (recur b 0 (inc j))
     :else nil)))

(defn solve-helper [board]
  (if
   (filled? board) board
   (let [p (find-empty-point board)]
     (for [x (valid-values-for board p)
           solutions (solve-helper (set-value-at board p x))]
       solutions))))

(defn solve [board]
  (solve-helper board))
