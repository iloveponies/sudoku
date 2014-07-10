(ns sudoku
  (:require [clojure.set :as set])
  (:require [clojure.math.combinatorics :as combo])
  )

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not ( = 0 (value-at board coord))))

(defn row-values [board [row col]]
  (set (get-in board [row])))

(defn col-values [board [row col]]
  (set (map #(get-in board [% col]) (range 0 9))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board [x y]]
  (let [startx (* 3 (quot x 3))
        starty (* 3 (quot y 3))
        rangex (range startx (+ startx 3))
        rangey (range starty (+ starty 3))
        coord-pairs (fn [coordsx coordsy]
                      (for [row coordsx
                            col coordsy]
                        [row col]))]
    (set (map (fn [x] (value-at board x)) (coord-pairs rangex rangey)))))

(defn valid-values-for [board coords]
  (if (has-value? board coords)
    #{}
    (set/difference
     #{1 2 3 4 5 6 7 8 9}
     (reduce set/union #{} [(block-values board coords)
                            (row-values board coords)
                            (col-values board coords)]))))

(defn rows [board]
  (map #(row-values board [% 0]) (range 9)))

(defn filled? [board]
  (not (some #(contains? % 0) (rows board))))

(defn valid-rows? [board]
  (every? #(and ( = 9 (count %1))
                (not (contains? %1 0)))
          (rows board)))

(defn cols [board]
  (map #(col-values board [0 %]) (range 9)))

(defn valid-cols? [board]
  (every? #(and ( = 9 (count %1))
                (not (contains? %1 0)))
          (cols board)))

(defn blocks [board]
  (for [row [0 4 8]
        col [0 4 8]]
    (block-values board [row col])))

(defn valid-blocks? [board]
  (every? #(and ( = 9 (count %1))
                (not (contains? %1 0)))
          (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter #(= 0 (value-at board %))
                 (coord-pairs [0 1 2 3 4 5 6 7 8]))))

(defn solve [board]
  (if (valid-solution? board)
    board
    (if (filled? board)
      false
      (let [pos (find-empty-point board)]
        (some solve (map #(set-value-at board pos %)
                         (valid-values-for board pos)))))))
