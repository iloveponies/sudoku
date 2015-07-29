(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (zero? (value-at board coord)) false true))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (loop [ret #{}
         n (dec (count board))]
    (if (< n 0)
      ret
      (recur (conj ret (value-at board [n (second coord)])) (dec n)))))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn get-start [n]
  (if (< n 3)
    0
    (if (< n 6)
      3
      6)))

(defn block-values [board coord]
  (loop [ret #{}
         x (get-start (first coord))
         y (get-start (second coord))
         x-lim (+ (get-start (first coord)) 2)
         y-lim (+ (get-start (second coord)) 2)]
    (if (and (== x x-lim) (== y y-lim))
      (conj ret (value-at board [x y]))
      (if (== x x-lim)
        (recur (conj ret (value-at board [x y])) (get-start (first coord)) (inc y) x-lim y-lim)
        (recur (conj ret (value-at board [x y])) (inc x) y x-lim y-lim)))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference #{1 2 3 4 5 6 7 8 9} (row-values board coord) (col-values board coord) (block-values board coord))))

(defn values-helper [board]
  (loop [ret #{}
         x 0
         y 0
         x-lim 8
         y-lim 8]
    (if (and (== x x-lim) (== y y-lim))
      (conj ret (value-at board [x y]))
      (if (== x x-lim)
        (recur (conj ret (value-at board [x y])) 0 (inc y) x-lim y-lim)
        (recur (conj ret (value-at board [x y])) (inc x) y x-lim y-lim)))))

(defn filled? [board]
  (if (contains? (values-helper board) 0)
    false
    true))

(defn rows [board]
  (loop [ret []
         n 0]
    (if (== n 9)
       ret
      (recur (conj ret (row-values board [n 0])) (inc n)))))

(defn valid-rows? [board]
  (loop [row-list (rows board)]
    (if (empty? row-list)
      true
      (if (empty? (set/difference #{1 2 3 4 5 6 7 8 9} (first row-list)))
        (recur (rest row-list))
        false))))

(defn cols [board]
  (loop [ret []
         n 0]
    (if (== n 9)
       ret
      (recur (conj ret (col-values board [0 n])) (inc n)))))

(defn valid-cols? [board]
  (loop [col-list (cols board)]
    (if (empty? col-list)
      true
      (if (empty? (set/difference #{1 2 3 4 5 6 7 8 9} (first col-list)))
        (recur (rest col-list))
        false))))

(defn blocks [board]
  (loop [ret []
         x 0
         y 0]
    (if (== y 9)
      ret
      (if (== x 6)
        (recur (conj ret (block-values board [y x])) 0 (+ y 3))
        (recur (conj ret (block-values board [y x])) (+ x 3) y)))))

(defn valid-blocks? [board]
  (loop [block-list (blocks board)]
    (if (empty? block-list)
      true
      (if (empty? (set/difference #{1 2 3 4 5 6 7 8 9} (first block-list)))
        (recur (rest block-list))
        false))))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [x 0
         y 0]
    (if (== y 9)
      []
      (if (not (has-value? board [x y]))
        [x y]
        (if (== x 8)
          (recur 0 (inc y))
          (recur (inc x) y))))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      [])
    (let [coords (find-empty-point board)]
      (for [elem (valid-values-for board coords)
            new-board (solve (set-value-at board coords elem))]
        new-board))))
