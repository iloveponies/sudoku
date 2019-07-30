(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
 (get-in board coord))

(defn has-value? [board coord]
  (< 0 (value-at board coord)))

(defn row-values [board coord]
  (let [[row _] coord]
        (set (get board row))))


(defn col-values [board coord]
  (loop [[_ col] coord
         values []
         n 8]
    (if (> 0 n)
      (set values)
      (recur coord (conj values (value-at board [n col])) (dec n)))))

(defn coord-pairs [coords]
  (for [c1 coords
        c2 coords]
    (conj [c1] c2)))

(defn upper-left [coord]
  (let [[x y] coord]
    [(- x (mod x 3)) (- y (mod y 3))]))

(defn block-coords [coord]
  (let [[x y] (upper-left coord)]
        (for [[a b] (coord-pairs [0, 1, 2])]
           [(+ a x) (+ b y)])))

(defn block-values [board coord]
  (set (map (fn [x] (value-at board x)) (block-coords coord))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
      #{}
      (set/difference (set/difference
                         (set/difference all-values
                                         (row-values board coord))
                         (col-values board coord))
                      (block-values board coord))))

(defn filled? [board]
 (every? pos? (apply concat board)))

(defn valid-sets [sets]
  (every? (fn [x] (= all-values x)) sets))

(defn rows [board]
  (for [n (range 9)]
       (row-values board [n 0])))

(defn valid-rows? [board]
  (valid-sets (rows board)))

(defn cols [board]
  (for [n (range 9)]
       (col-values board [0 n])))

(defn valid-cols? [board]
  (valid-sets (cols board)))

(defn blocks [board]
  (for [bls (coord-pairs [0 3 6])]
       (block-values board bls)))

(defn valid-blocks? [board]
  (valid-sets (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (fn [x] (not (has-value? board x))) (coord-pairs (range 9)))))

(defn solve-helper [board]
  (if (filled? board)
      (if (valid-solution? board)
          [board]
          [])
      (let [now (find-empty-point board)]
          (for [valids (valid-values-for board now)
                solution (solve-helper (set-value-at board now valids))]
                solution))))

(defn solve [board]
  (first (solve-helper board)))


; Recap of backtracking:
; check if you are at the end
; if so, is the solution valid?
; if not, return an empty sequence
;   otherwise return [solution]
; if not
;  select an empty location
;  try solving with each valid value for that location
