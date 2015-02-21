(ns sudoku
  (:require [clojure.set :as set]))

(def all-values #{1 2 3 4 5 6 7 8 9})

(def board identity)

(defn value-at [board coord]
  (get-in board coord))


(defn has-value? [board coord]
  (not= (value-at board coord) 0))


(defn row-values [board coord]
  (reduce #(conj %1 %2) (set()) (get-in board (conj [] (first coord)))))

(defn col-values [board coord]
  (loop [row 0
         ret-set (set ())]
    (cond
     (= row 9) ret-set
     :else (recur (inc row) (conj ret-set (value-at board (conj (conj [] row) (first (rest coord))))))
    )))

(defn coord-pairs [coords]
    (for [x coords
          y coords]
         (conj (conj [] x) y)))

(defn top-left-coords [coords]
  [(* (int (/ (first coords) 3)) 3) (* (int (/ (first (rest coords)) 3)) 3)])


	  (defn block-values [board coord]
  (let [startY (first (top-left-coords coord))
        startX (first (rest (top-left-coords coord)))]
  (loop [ret-set '()
         curY startY
          curX startX]
    (cond
     (and (= (- curY startY) 2) (= (- curX startX) 2)) (set (conj ret-set (value-at board [curY curX])))
      (= (- curY startY) 2) (recur (conj ret-set (value-at board [curY curX])) startY (inc curX))
      :else (recur (conj ret-set (value-at board [curY curX])) (inc curY) curX)))))

(defn valid-values-for [board coord]
  (if (not= (value-at board coord) 0)
    #{}
    (clojure.set/difference all-values (clojure.set/union (col-values board coord) (row-values board coord) (block-values board coord)))))

(defn all-in-board [board]
  (loop [ret-set #{}
         cur-col 0]
    (cond
     (= cur-col 9) ret-set
     :else (recur (clojure.set/union ret-set (col-values board [0 cur-col])) (inc cur-col))
    )))

(defn filled? [board]
  (not (contains? (all-in-board board) 0)))

(defn rows [board]
    (loop [ret-seq '()
         cur-row 0]
    (cond
     (= cur-row 9) (reverse ret-seq)
     :else (recur (conj ret-seq (row-values board [cur-row 0])) (inc cur-row))
    )))

(defn set-valid? [row-set]
  (loop [row row-set
         all all-values]
    (cond
     (empty? all) true
     (not (contains? row (first all))) false
     :else (recur row (rest all)))))

(defn valid-rows? [board]
  (loop [rows (rows board)]
    (cond
     (empty? rows) true
     (not (set-valid? (first rows))) false
     :else (recur (rest rows)))))

(defn cols [board]
    (loop [ret-seq '()
         cur-col 0]
    (cond
     (= cur-col 9) (reverse ret-seq)
     :else (recur (conj ret-seq (col-values board [0 cur-col])) (inc cur-col))
    )))

(defn valid-cols? [board]
  (loop [cols (cols board)]
    (cond
     (empty? cols) true
     (not (set-valid? (first cols))) false
     :else (recur (rest cols)))))

(defn blocks [board]
  (loop [ret-seq '()
         curY 0
         curX 0]
    (cond
     (and (= curY 6) (= curX 6)) (reverse (conj ret-seq (block-values board [curY curX])))
     (= curX 6) (recur (conj ret-seq (block-values board [curY curX])) (+ curY 3) 0)
     :else (recur (conj ret-seq (block-values board [curY curX])) curY (+ curX 3)))))

(defn valid-blocks? [board]
  (loop [blocks (blocks board)]
    (cond
     (empty? blocks) true
     (not (set-valid? (first blocks))) false
     :else (recur (rest blocks)))))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  nil)

(defn solve [board]
  nil)
