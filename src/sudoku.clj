(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (= 0 (value-at board coord))
    false
    true))

(defn row-values [board [row _]]
  (set (get board row)))

(defn col-values [board [_ col]]
  (let [browser (fn [acc iii]
                  (conj acc (value-at board [iii col])))]
    (reduce browser #{} (range 0 9))))

(defn coord-pairs [coords]
  (vec
    (for [x coords
          y coords]
      [x y])))

(defn block-values [board coord]
  (let [block-coord   (fn [[x y]]
                        [(* (quot x 3) 3) (* (quot y 3) 3)])
        [currX currY] (block-coord coord)
        pairs         (for [x (range currX (+ currX 3))
                            y (range currY (+ currY 3))]
                        [x y])
        browser       (fn [acc coord]
                        (conj acc (value-at board coord)))]
    (reduce browser #{} pairs)))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                    (block-values board coord)
                    (row-values board coord)
                    (col-values board coord))))

(defn filled? [board]
  (let [browser (fn [acc row]
                  (if (some zero? row)
                    (reduced false)
                    true))]
    (reduce browser true board)))

(defn rows [board]
  (for [iii (range 0 9)]
    (row-values board [iii 0])))

(defn valid-rows? [board]
  (let [browser (fn [acc row]
                  (if (and (= (count row) 9)
                           (empty? (set/difference row all-values)))
                    true
                    (reduced false)))]
    (reduce browser true (rows board))))

(defn cols [board]
  (for [iii (range 0 9)]
    (col-values board [0 iii])))

(defn valid-cols? [board]
  (let [browser (fn [acc col]
                  (if (and (= (count col) 9)
                           (empty? (set/difference col all-values)))
                    true
                    (reduced false)))]
    (reduce browser true (cols board))))

(defn blocks [board]
  (let [browser (fn [acc coord]
                 (conj acc (block-values board coord)))]
    (reduce browser [] (coord-pairs [0 3 6]))))

(defn valid-blocks? [board]
  (let [browser (fn [acc block]
                  (if (and (= (count block) 9)
                           (empty? (set/difference block all-values)))
                    true
                    (reduced false)))]
    (reduce browser true (blocks board))))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [index-of (fn [s e]
                   (map first
                        (filter #(= e (second %))
                                (map-indexed vector s))))
        browser  (fn [x row]
                  (let [idxs (index-of row 0)]
                    (if (empty? idxs)
                      (inc x)
                      (reduced [x (first idxs)]))))
        coord    (reduce browser 0 board)]
    (if (number? coord)
      nil
      coord)))

(defn solve-helper [board]
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      '())
    (let [empty-loc (find-empty-point board)]
      (for [valid-val (valid-values-for board empty-loc)
            solution  (solve-helper (set-value-at board
                                                  empty-loc
                                                  valid-val))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))
