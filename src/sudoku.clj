(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board coord]
  (let [row (get coord 0)
        colinds (range 0 9)]
    (loop [ind 0
           nums #{}
           inds colinds]
      (if (empty? inds)
        nums
        (recur (inc ind)
               (conj nums (value-at board [row ind]))
               (rest inds))))))

(defn col-values [board coord]
  (let [col (get coord 1)
        rowinds (range 0 9)]
    (loop [ind 0
           nums #{}
           inds rowinds]
      (if (empty? inds)
        nums
        (recur (inc ind)
               (conj nums (value-at board [ind col]))
               (rest inds))))))

(defn coord-pairs [coords]
  (def jea [])
  (for [coo coords
        cooo coords]
    (conj jea coo cooo)))

(defn get-corner [coord]
  (let [[x y] coord]
    (cond
      (< 2 x 6) (cond
                  (< 2 y 6) [3 3]
                  (> y 5) [3 6]
                  :else [3 0])
      (> x 5) (cond
                (< 2 y 6) [6 3]
                (> y 5) [6 6]
                :else [6 0])
      :else (cond
              (< 2 y 6) [0 3]
              (> y 5) [0 6]
              :else [0 0]))))

(defn coord-pairs-another [coords cooords]
  (def jea [])
  (for [coo coords
        cooo cooords]
    (conj jea coo cooo)))

(defn block-values [board coord]
  (let [corner (get-corner coord)
        cornx (get corner 0)
        corny (get corner 1)
        cpairs (coord-pairs-another (range cornx (+ cornx 3))
                                    (range corny (+ corny 3)))]
    (set (map (fn [x] (value-at board x)) cpairs))))

(defn valid-values-for [board coord]
  (let [setti (set/union (block-values board coord)
                         (row-values board coord)
                         (col-values board coord))]
    (if (has-value? board coord)
      #{}
      (set/difference #{1 2 3 4 5 6 7 8 9} setti))))

(defn all-values [board]
  (let [cp (coord-pairs [0 3 6])]
    (apply set/union (map (fn [x] (set/union (block-values board x))) cp))))

(defn filled? [board]
  (let [values (all-values board)]
    (if (contains? values 0)
      false
      true)))

(defn rows [board]
  (loop [ind 0
         rs []]
    (if (== ind 9)
      rs
      (recur (inc ind) (conj rs (row-values board [ind ind]))))))

(defn valid-rows? [board]
  (every? identity (map (fn [x] (= x #{1 2 3 4 5 6 7 8 9})) (rows board))))

(defn cols [board]
  (loop [ind 0
         cs []]
    (if (== ind 9)
      cs
      (recur (inc ind) (conj cs (col-values board [ind ind]))))))

(defn valid-cols? [board]
  (every? identity (map (fn [x] (= x #{1 2 3 4 5 6 7 8 9})) (cols board))))

(defn blocks [board]
  (let [cp (coord-pairs [0 3 6])]
    (map (fn [x] (set/union (block-values board x))) cp)))

(defn valid-blocks? [board]
  (every? identity (map (fn [x] (= x #{1 2 3 4 5 6 7 8 9})) (blocks board))))

(defn valid-solution? [board]
  (if (and (valid-rows? board)
           (valid-cols? board)
           (valid-blocks? board))
    true
    false))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [cps (coord-pairs [0 1 2 3 4 5 6 7 8])]
    (loop [eka (first cps)
           loput (rest cps)
           nollat []]
      (if (= nil eka)
        nil
        (if (has-value? board eka)
          (recur (first loput) (rest loput) nollat)
          eka)))))

(defn solve-h [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      [])
    (let [first-zero (find-empty-point board)
          valids (valid-values-for board first-zero)]
      (flatten (for [v valids]
                 (solve-h (set-value-at board first-zero v)))))))

(defn solve [board]
  (loop [board-seq (solve-h board)
         constr-board []]
    (if (empty? board-seq)
      constr-board
      (recur (drop 9 board-seq)
             (conj constr-board (take 9 board-seq))))))
