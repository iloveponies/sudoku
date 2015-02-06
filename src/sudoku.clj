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
  (reduce (fn [acc e] (conj acc (get e (second coord)))) #{} board))

(defn coord-pairs [coords]
  (for [c1 coords
        c2 coords]
    [c1 c2]))

(defn block-corner [coords]
  (let [[c1 c2] coords] 
    [(- c1 (mod c1 3)) (- c2 (mod c2 3))]))

(defn block-coords [coords]
  (let [c (block-corner coords)
        y (for [n [0 1 2]] (+ n (first c)))
        x (for [n [0 1 2]] (+ n (second c)))]
    (for [c1 y
          c2 x]
      [c1 c2])))

(defn block-values [board coord]
  (let [coords (block-coords coord)]
    (reduce (fn [acc e] (conj acc (value-at board e))) #{} coords)))

(defn valid-values-for [board coord]
 (let [r (row-values board coord)
       c (col-values board coord)
       b (block-values board coord)
       all (set/union r c b)]
   (if (not (= 0 (value-at board coord)))
     #{}
     (set/difference all-values all))))

(defn filled? [board]
  (not (contains? (set (flatten board)) 0)))

(defn valid? [f board]
  (let [remaining #(set/difference all-values %)] 
    (every? identity (map empty? (map remaining (f board))))))

(defn rows [board]
  (for [r board]
    (set r)))

(defn valid-rows? [board]
  (valid? rows board))

(defn cols [board]
  (for [i (range 0 9)]
    (set (col-values board [0 i]))))

(defn valid-cols? [board]
  (valid? cols board))

(defn blocks [board]
  (let [corners (for [x [0 3 6]
                      y [0 3 6]]
                  [x y])
        blocks (for [c corners] 
                 (block-values board c))]
    (map set blocks)))

(defn valid-blocks? [board]
  (valid? blocks board))

(defn valid-solution? [board]
  (and (valid-cols? board) (valid-rows? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [coords (for [y (range 0 9)
                     x (range 0 9)]
                 [y x])]
    (first (filter #(= 0 (value-at board %)) coords))))

(defn find-empty-points [board]
  (let [coords (for [y (range 0 9)
                     x (range 0 9)]
                 [y x])]
    (filter #(= 0 (value-at board %)) coords)))

(defn solve [board]
  (if (valid-solution? board)
    [board]
    (let [e (find-empty-point board)
          valid-values (valid-values-for board e)]
      (partition 9 (flatten (for [v valid-values] 
                              (solve (set-value-at board e v))))))))
