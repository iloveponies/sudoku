(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
 (get-in board coord))


(defn has-value? [board coord]
  (> (value-at board coord) 0))

(defn row-values [board coord]
  (let [[row col] coord] (set (get board row))))


(defn col-values [board [row col]]
 (set (map #(get % col) board)))


(defn coord-pairs [coords]
  (seq (for [row coords col coords] (vector row col))))

(defn block-values [board coord]
  (let [top-lft (fn [crd] (let [[row col] crd] (vector (- row (mod row 3)) (- col (mod col 3)))))
        first-cell (top-lft coord)
        first-r (get first-cell 0)
        first-c (get first-cell 1)
        row-range (range first-r (+ 3 first-r))
        col-range (range first-c (+ 3 first-c))
        coord-seq (seq (for [rs row-range cs col-range ] (vector rs cs)))
       ] (set (map (fn [x] (value-at board x)) coord-seq
                   ))))


(defn valid-values-for [board coord]
  (if (zero? (value-at board coord))
  (set/difference all-values (row-values board coord) (col-values board coord)(block-values board coord))
  #{}))


(defn filled? [board]
  (let [vals (seq (for [r (range 0 9) c (range 0 9)] (value-at board (vector r c))))]
    (not (contains? (set vals) 0)))
  )

(defn rows [board]
  (into [] (for [r (range 0 9)]
            (set (row-values board [r 0])))))


(defn valid-rows? [board]
  (let [all-rows (rows board)] (every? (fn [x] (= all-values x)) all-rows)))


(defn cols [board]
   (into [] (for [c (range 0 9)] (set (col-values board [0 c])))))



(defn valid-cols? [board]
  (let [all-cols (cols board)] (every? (fn [x] (= all-values x)) all-cols)))


;; want to get each block
;; block at
;; [0 0] [0 3] [0 6]
;; [3 0] [3 3] [3 6]
;; [6 0] [6 3] [6 6]

(defn blocks [board]
  (into [] (for [r [0 3 6] c [0 3 6]] (block-values board [r c]))))



(defn valid-blocks? [board]
  (let [all-blocks (blocks board)] (every? (fn [x] (= all-values x)) all-blocks)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))


(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))


(defn find-empty-point [board]
  (let [pairs (coord-pairs (range 0 9))]
    (first (filter #(zero? (value-at board %)) pairs))))


(defn solve-helper [a-board]
  (if (filled? a-board)
    (if (valid-solution? a-board) [a-board] [])
    ;; need to backtrack
    (let [next-pos-to-try (find-empty-point a-board)]
      (for [valid-value (valid-values-for a-board next-pos-to-try)
            solution (solve-helper (set-value-at a-board next-pos-to-try valid-value))]
        solution))))

(defn solve [my-board]
  (first (solve-helper my-board)))

