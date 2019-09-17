(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  ((complement zero?) (value-at board coord)))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (set (map #(get % (second coord)) board)))

(defn coord-pairs [coords]
  (for [a coords
        b coords]
        [a b]))

(defn block-range [block]
  (let [[x y] block
        top-left-x (* (int (/ x 3)) 3)
        top-left-y (* (int (/ y 3)) 3)]
    [top-left-x top-left-y
     (+ 3 top-left-x) (+ 3 top-left-y)]))
    
(defn block-values [board coord]
  (let [[x1 y1 x2 y2] (block-range coord)]
    (set
      (for [x (range x1 x2)
            y (range y1 y2)]
        (value-at board [x y])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference
      all-values
      (block-values board coord)
      (row-values board coord)
      (col-values board coord))))

(defn filled? [board]
  (every? #(not= % 0) (reduce concat board)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? #(= all-values %) (rows board)))

(defn cols [board]
  (map set (apply map list board)))

(defn valid-cols? [board]
  (every?  #(= all-values %) (cols board)))

(defn blocks [board]
  (for [x (range 0 3)
        y (range 0 3)]
    (block-values board [(* 3 x) (* 3 y)])))

(defn valid-blocks? [board]
  (every? #(= all-values %) (blocks board)))

(defn valid-solution? [board]
  (and (valid-blocks? board)
    (valid-cols? board)
    (valid-rows? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn all-points [board]
  (for [x (range 0 9)
        y (range 0 9)]
    [x y]))

(defn find-empty-point [board]
  (first (filter
    #(zero? (value-at board %))
    (all-points board))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board) board [])
    (let [test-location (find-empty-point board)]
      (for [test-value (valid-values-for board test-location)
            result (solve (set-value-at board test-location test-value))]
        result
    ))))
