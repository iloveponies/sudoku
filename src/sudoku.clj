(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not(zero? (value-at board coord))))

(defn row-values [board [row col]]
  (loop [r #{}
         i 0]
    (if (< i 9) (recur 
                  (conj r (value-at board [row, i]))
                  (inc i))
      r)))

(defn col-values [board [row col]]
  (loop [r #{}
         i 0]
    (if (< i 9) (recur 
                  (conj r (value-at board [i, col]))
                  (inc i))
      r)))


(defn coord-pairs [coords]
  (for [x coords
        y coords]
    (vec [x y])))

(defn top-left [[row col]] 
  (vec [(* (unchecked-divide-int row 3) 3)
        (* (unchecked-divide-int col 3) 3)]
       ))

(defn block-values [board coord]
  (let [[row col] (top-left coord)]
    (set (for [i (range 0 3)
               j (range 0 3)]
           (value-at board [(+ row i) (+ col j)])
           ))))

(defn valid-values-for [board coord]
  (let [numbers (set (range 1 10))]
    (if (> (value-at board coord) 0) 
      #{}
      (clojure.set/difference numbers
                              (block-values board coord)
                              (row-values board coord)
                              (col-values board coord)
                              ))))

;; Moved this to use find-empty-point (minimize code copy-paste)
;; (defn filled? [board]
;;   (loop [i 0]
;;     (let [row (unchecked-divide-int i 9) 
;;           col (mod i 9)]
;;       (cond 
;;         (= 0 (value-at board [row col])) false
;;         (= row 9) true
;;         :else (recur (inc i))
;;         ))))

(defn rows [board]
  (for [row (range 0 9)]
    (row-values board [row 0])))

(defn cols [board]
  (for [col (range 0 9)]
    (col-values board [0 col])))

(defn blocks [board]
  (for [row (range 0 3)
        col (range 0 3)]
    (block-values board [(* row 3) (* col 3)])))

(defn has-all-values? [values]
  (empty? (clojure.set/difference (set (range 1 10)) values)))

(defn valid-rows? [board]
  (every? has-all-values? (rows board)))

(defn valid-cols? [board]
  (every? has-all-values? (cols board)))

(defn valid-blocks? [board]
  (every? has-all-values? (blocks board)))

(defn valid-solution? [board]
  (and (valid-blocks? board)
       (valid-rows? board)
       (valid-cols? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [i 0]
    (let [row (unchecked-divide-int i 9) 
          col (mod i 9)]
      (cond 
        (= 0 (value-at board [row col])) [row col]
        (= row 9) nil
        :else (recur (inc i))
        ))))

(defn filled? [board] (if (find-empty-point board) false true))

(defn solve [board]
  (if (valid-solution? board) 
    board
    (let [empty-point (find-empty-point board)]
      (if (empty? empty-point)
        #{}
        (loop [value 1] 
          (let [solution (solve (set-value-at board empty-point value))]
            (cond 
              (not-empty solution) solution
              (= value 9) #{}
              :else (recur (inc value)))))))))


;; (defn solve [board]
;;   (let [empty-point (find-empty-point board)]
;;     (cond
;;       (not-empty empty-point) (loop [value 1]
;;                                 (let [solution (solve (set-value-at board empty-point value))]
;;                                   (cond 
;;                                     (not-empty solution) solution
;;                                     (> value 9) nil
;;                                     :else (recur (inc value)))))
;;       (valid-solution? board) board
;;       :else #{})))

