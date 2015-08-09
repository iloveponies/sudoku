(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (= 0 (get-in board coord))
    false
    true))

(defn row-values [board coord]
  (reduce
   #(
     if (contains? %1 %2)
     %1
     (conj %1 %2))
   #{}
   (let
     [[x y] coord]
     (get board x))))

(defn col-values [board coord]
  (let [[x y] coord]
    (reduce
     #(
       if (contains? %1 (get %2 y))
       %1
       (conj %1 (get %2 y)))
     #{} board)))

;toteuttaa kaikki kombinaatiot
(defn coord-pairs [coords]
  (for [x coords y coords]
    [x y]))

(defn a-block-range-by [x]
  (cond
   (< x 3) (range 0 3)
   (and (>= x 3) (< x 6)) (range 3 6)
   (and (>= x 6) (< x 9)) (range 6 9)
   :else nil))

(defn a-block-coordinates [board row col]
  (for [x (a-block-range-by row)
        y (a-block-range-by col)]
    [x y]))

(defn block-values [board coord]
  (let [[row col] coord
        coord-vals (fn [a-coord] (value-at board a-coord))]
    (set (map coord-vals (a-block-coordinates board row col)))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [a-row (row-values board coord)
          a-col (col-values board coord)
          a-block (block-values board coord)
          valid-for-a-block (set/difference all-values a-block)
          valid-for-a-col (set/difference all-values a-col)
          valid-for-a-row (set/difference all-values a-row)]
      (set/intersection valid-for-a-block valid-for-a-col valid-for-a-row))))

;flatten vain sekvensseille (yhdistaa sekvenssin sisaiset sekvenssit toisiinsa)
(defn filled? [board]
  (let [all-set (into #{} (flatten board))]
    (not (contains? all-set 0))))

(defn valid? [a-set]
  (empty?
   (filter #(not (empty? (set/difference all-values %1))) a-set)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (valid? (rows board)))

(defn cols [board]
  (for [col (range 0 (count board))]
    (col-values board [0 col])))

(defn valid-cols? [board]
  (valid? (cols board)))

(defn blocks [board]
  (reduce #(conj %1 (block-values board %2)) [] (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (valid? (blocks board)))

(defn valid-solution? [board]
  (and (valid-blocks? board) (valid-cols? board) (valid-rows? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

;muodostetaan kaikkien sudokutaulun koordinaattien joukko ja
;poistetaan siita kaikki ne koordinaatit, joissa on arvo
(defn find-all-empty-points [board]
  (filter #(not (has-value? board %1)) (coord-pairs (range (count board)))))

(defn find-empty-point [board]
  (first (find-all-empty-points board)))

;jollei ratkaisua, palauttaa []
;brute force -ratkaisu backtracking searchilla
  (if (filled? board)
    (if (valid-solution? board)
      board
      [])
    (let [empty-point (find-empty-point board)]
      (for [possible-value
            (valid-values-for board empty-point)
            solution (solve (assoc-in board empty-point possible-value))]
        solution))))
