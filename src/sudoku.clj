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

(defn row-values [board coord]
  (let [[x y] coord]
  (set (get board x))))

(defn col-values [board coord]
  (let [[x y] coord]
   (loop [i 0
          vastaus (set ())]
    (if (= i 9)
      vastaus
      (recur (inc i) (conj vastaus (value-at board [i y])))))))

(defn coord-pairs [coords]
  (let [[x y] coords
        vastaus []]
    (for [eka coords
          toka coords]
       (conj vastaus eka toka))))

(defn apu [coord]
  (let [[x y] coord]
    (if (and (< x 3) (< y 3))
      [0 0]
      (if (and (< x 6) (< y 3))
        [3 0]
        (if (and (< x 9) (< y 3))
          [6 0]
          (if (and (< x 3) (< y 6))
            [0 3]
            (if (and (< x 6) (< y 6))
              [3 3]
              (if (and (< x 9) (< y 6))
                [6 3]
                (if (and (< x 3) (< y 9))
                  [0 6]
                  (if (and (< x 6) (< y 9))
                    [3 6]
                    (if (and (< x 9) (< y 9))
                      [6 6]
                      false)))))))))))

(defn block-values [board coord]
  (let [[x y] coord
        [xx yy] (apu coord)
        coords [[xx yy] [(+ 1 xx) yy] [(+ 2 xx) yy]
                [xx (+ 1 yy)] [(+ 1 xx) (+ 1 yy)] [(+ 2 xx) (+ 1 yy)]
                [xx (+ 2 yy)] [(+ 1 xx) (+ 2 yy)] [(+ 2 xx)(+ 2 yy)]]
        oikeet (for [nama coords]
                 (value-at board nama))]
       (set (seq oikeet))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                  (set/union
                    (row-values board coord)
                    (col-values board coord)
                    (block-values board coord)))))

(defn apupaska [board]
  (for [rivi board]
   (let [mitvit (set rivi)
         vastaus #{}]
     (if (contains? mitvit  0)
       false
       true))))

(defn filled? [board]
   (if (contains? (set (apupaska board)) false)
     false
     true))



(defn rows [board]
    (loop [i 0
           vastaus []]
      (if (= i 9)
        vastaus
        (recur (inc i) (conj vastaus (row-values board [i 0]))))))

(defn apu-rows [board]
  (let [kaikki (rows board)]
    (for [tama kaikki]
      (empty? (seq (set/difference all-values tama))))))

(defn valid-rows? [board]
   (let [vastaus (fn[] (if (contains? (set (apu-rows board)) false)
                         false
                         true))]
     (and (filled? board)(vastaus))))

(defn cols [board]
  (loop [i 0
           vastaus []]
      (if (= i 9)
        vastaus
        (recur (inc i) (conj vastaus (col-values board [0 i]))))))

(defn apu-cols [board]
  (let [kaikki (cols board)]
    (for [tama kaikki]
      (empty? (seq (set/difference all-values tama))))))


(defn valid-cols? [board]
   (let [vastaus (fn[] (if (contains? (set (apu-cols board)) false)
                         false
                         true))]
     (and (filled? board)(vastaus))))


(defn blocks [board]
    (let [kaikki [[0 0] [0 3] [0 6] [3 0] [3 3] [3 6] [6 0] [6 3] [6 6]]
          oikeet (for [tama kaikki]
                    (block-values board tama))]
      (into [] oikeet)))

(defn apu-blocks [board]
  (let [kaikki (blocks board)]
    (for [tama kaikki]
      (empty? (seq (set/difference all-values tama))))))

(defn valid-blocks? [board]
  (let [vastaus (fn[] (if (contains? (set (apu-blocks board)) false)
                         false
                         true))]
     (and (filled? board)(vastaus))))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [coords (coord-pairs [1 2 3 4 5 6 7 8 9])]
    (first (filter (fn [coord] (not (has-value? board coord))) coords))))

(defn solve [board]
  nil)
