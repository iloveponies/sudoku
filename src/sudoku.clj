(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

;; (def sudoku-board
;;   (board [[5 3 0 0 7 0 0 0 0]
;;           [6 0 0 1 9 5 0 0 0]
;;           [0 9 8 0 0 0 0 6 0]
;;           [8 0 0 0 6 0 0 0 3]
;;           [4 0 0 8 0 3 0 0 1]
;;           [7 0 0 0 2 0 0 0 6]
;;           [0 6 0 0 0 0 2 8 0]
;;           [0 0 0 4 1 9 0 0 5]
;;           [0 0 0 0 8 0 0 7 9]]))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (zero? (value-at board coord))
    false
    true))

(defn row-coords [coord]
  (let [[row col] coord]
    (map (fn [c] (vector row c)) (range 9))))

(defn coords-values [board coords]
  (into #{} (map #(value-at board %)
                 coords)))

(defn row-values [board coord]
  (coords-values board (row-coords coord)))

(defn col-coords [coord]
  (let [[row col] coord]
    (map (fn [r] (vector r col)) (range 9))))

(defn col-values [board coord]
  (coords-values board (col-coords coord)))

(defn coord-pairs [coords]
  (into [] (for [r coords
                 c coords]
             (vector r c))))

(defn anchor
  "Find the top/left row/col corresponding to n"
  [n]
  (int (* 3 (Math/floor (/ n 3)))))

(defn block-coords [coord]
  (let [[row col] coord
        r (anchor row)
        c (anchor col)]
    (into [] (for [rw (range r (+ r 3))
                   cl (range c (+ c 3))]
               (vector rw cl)))))

(defn block-values [board coord]
  (coords-values board (block-coords coord)))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [cell-valid (set/difference (set (range 1 10))
                                     (block-values board coord))
          row-valid (set/difference cell-valid
                                    (row-values board coord))
          col-valid (set/difference row-valid
                                    (col-values board coord))]
      col-valid)))

(defn all-coords [board]
  (let [vals (range (count board))]
    (into [] (for [r vals
                   c vals]
               (vector r c)))))

(defn filled? [board]
  (let [coords (all-coords board)]
    (every? (complement zero?) (map #(value-at board %) coords))))

(defn rows [board]
  (into [] (for [r (range 9)]
             (row-values board [r 0]))))

(defn valid-rows? [board]
  (apply = #{1 2 3 4 5 6 7 8 9} (rows board)))

(defn cols [board]
  (into [] (for [c (range 9)]
             (col-values board [0 c]))))

(defn valid-cols? [board]
  (apply = #{1 2 3 4 5 6 7 8 9} (cols board)))

(defn blocks [board]
  (let [coord-vals (range 0 9 3)]
    (into [] (map #(block-values board %) (coord-pairs coord-vals)))))

(defn valid-blocks? [board]
  (apply = #{1 2 3 4 5 6 7 8 9} (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter #(zero? (value-at board %)) (all-coords board))))

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      nil)
    (let [coord (find-empty-point board)]
      (for [val (valid-values-for board coord)
            sol (solve-helper (set-value-at board coord val))]
        sol))))

(defn solve [board]
  (first (solve-helper board)))

(defn sum [a-seq]
  (reduce + a-seq))

(defn subset-sum-helper [a-set current-set target]
  (if (= (sum current-set) target)
    [current-set]
    (let [remaining (clojure.set/difference a-set current-set)]
      (for [elem remaining
            solution (subset-sum-helper a-set
                                        (conj current-set elem)
                                        target)]
        solution))))

(defn subset-sum [a-set target]
  (subset-sum-helper a-set #{} target))
