(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (= 0 (get-in board coord)) false true))

(defn row-values [board coord]
  (let [rivi (get board (first coord))]
    (set rivi)))

(defn col-values [board coord]
  (let [kol(for [rivi board]
             (get rivi (second coord)))]
    (set kol)))

(defn coord-pairs [coords]
  (for [a coords b coords]
    (vector a b)))

(defn top-left [coords]
  (vector (* 3(int (Math/floor (/ (first coords) 3))))
          (* 3(int (Math/floor (/ (second coords) 3))))))

(defn block-values [board coord]
  (let [vasuri (top-left coord)]
  (set (for [i (range 3) j (range 3)]
    (let [eka (+ i (first vasuri))
          toka (+ j (second vasuri))]
    (value-at board [eka toka]))))))

(defn valid-values-for [board coord]
  (cond (has-value? board coord) #{}
        :else (set/difference
                all-values
                (block-values board coord)
                (row-values board coord)
                (col-values board coord))))

(defn all? [rivi]
  (let [apu (set rivi)]
    (if (= apu all-values) true false)))

(defn filled? [board]
  (let [totuusrivi (set (map all? board))]
    (if (contains? totuusrivi false) false true)))

(defn rows [board]
  (for [i (range 9)]
    (row-values board [i 0])))

(defn valid-rows? [board]
  (filled? board))

(defn cols [board]
  (for [j (range 9)]
    (col-values board [0 j])))

(defn valid-cols? [board]
  (filled? (cols board)))

(defn blocks [board]
  (for [i (range 3) j (range 3)]
    (block-values board [(* 3 i) (* 3 j)])))

(defn valid-blocks? [board]
  (filled? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-first-index [pred a-seq]
  (let [helper (fn [p sq n]
               (cond (empty? sq) nil
                     (p (first sq)) n
                     :else (recur p (rest sq) (inc n))))]
        (helper pred a-seq 0)))

(defn find-empty-point [board]
  (let [sekvenssi (for [i (range 9) j (range 9)]
    (has-value? board [i j]))]
    (vector (int (Math/floor (/ (find-first-index false? sekvenssi) 9)))
            (mod (find-first-index false? sekvenssi) 9))))

(defn solve [board]
    (cond (valid-solution? board) board
          (filled? board) nil
    :else (let [empty-point (find-empty-point board)]
      (for [possible-insertion (valid-values-for board empty-point)
            solution (solve (set-value-at board empty-point possible-insertion))]
        solution))))
