(ns sudoku
  (:require [clojure.set :as set]))
(def all-values #{1 2 3 4 5 6 7 8 9})
(def board identity)
;(def sudoku-board
  ;(board [[5 3 0 0 7 0 0 0 0]
          ;[6 0 0 1 9 5 0 0 0]
          ;[0 9 8 0 0 0 0 6 0]
          ;[8 0 0 0 6 0 0 0 3]
          ;[4 0 0 8 0 3 0 0 1]
          ;[7 0 0 0 2 0 0 0 6]
          ;[0 6 0 0 0 0 2 8 0]
          ;[0 0 0 4 1 9 0 0 5]
          ;[0 0 0 0 8 0 0 7 9]]))
;(def solved-board
  ;(board [[5 3 4 6 7 8 9 1 2]
          ;[6 7 2 1 9 5 3 4 8]
          ;[1 9 8 3 4 2 5 6 7]
          ;[8 5 9 7 6 1 4 2 3]
          ;[4 2 6 8 5 3 7 9 1]
          ;[7 1 3 9 2 4 8 5 6]
          ;[9 6 1 5 3 7 2 8 4]
          ;[2 8 7 4 1 9 6 3 5]
          ;[3 4 5 2 8 6 1 7 9]]))
(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (set (map value-at (repeat board) (for [r (range 9)]
                                      [r (second coord)]))))

(defn coord-pairs [coords]
  (for [a coords
        b coords]
    [a b]))

(defn block-values [board coord]
  (let [r-start (* (int (/ (first coord) 3)) 3)
        r-end (+ r-start 3)
        c-start (* (int (/ (second coord) 3)) 3)
        c-end (+ c-start 3)]
    (set (map value-at (repeat board) (for [r (range r-start r-end)
                                            c (range c-start c-end)]
                                        [r c])))))
(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values 
                    (row-values board coord)
                    (col-values board coord)
                    (block-values board coord))))

(defn filled? [board]
  (every? empty? (map valid-values-for (repeat board) (coord-pairs (range 9)))))

(def check? (partial every? #(= % all-values)))

(defn rows [board]
  (map row-values (repeat board) (for [i (range 9)]
                          [i 0])))

(defn valid-rows? [board]
  (check? (rows board)))

(defn cols [board]
  (map col-values (repeat board) (for [i (range 9)]
                          [0 i])))

(defn valid-cols? [board]
  (check? (cols board)))

(defn blocks [board]
  (map block-values (repeat board) (for [i (range 3)
                                         j (range 3)]
                                     [(* 3 i) (* 3 j)])))

(defn valid-blocks? [board]
  (check? (blocks board)))

(defn valid-solution? [board]
  (every? true?  (map #(%1 %2) 
                      [valid-cols? valid-rows? valid-blocks?]
                      (repeat board))))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter #(not (has-value? board %)) (coord-pairs (range 9)))))

(defn solve [board]
  (if (valid-solution? board)
    board
    (let [empty-point (find-empty-point board)]
      (for [elem (valid-values-for board empty-point)
            solution (solve (set-value-at board empty-point elem))]
        solution))))

