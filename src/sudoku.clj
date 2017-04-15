(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [row col]]
  (loop [i 0
         result #{}]
    (if (== i 9)
      result
      (recur (inc i) (conj result (value-at board [row i]))))))

(defn col-values [board [row col]]
  (loop [i 0
         result #{}]
    (if (== i 9)
      result
      (recur (inc i) (conj result (value-at board [i col]))))))

(defn coord-pairs [coords]
  (apply vector (for [row coords
                      col coords]
                  (conj [row] col))))

(defn block-values [board [row col]]
  (let [mult3 (fn [x] (* 3 (int (/ x 3))))
        block-coords [(mult3 row) (mult3 col)]
        all-cords (reduce (fn [acc item] (conj acc (apply vector (map + block-coords item)))) [] (coord-pairs [0 1 2]))]
    (loop [result #{}
           pos 0]
      (if (== pos 9)
        result
        (recur (conj result (value-at board (get all-cords pos))) (inc pos))))))

(defn valid-values-for [board [row col]]
  (if (has-value? board [row col])
    #{}
    (let [values-in-row (row-values board [row col])
          values-in-col (col-values board [row col])
          values-in-block (block-values board [row col])
          invalid-values (clojure.set/union values-in-row values-in-col values-in-block)]
      (clojure.set/difference all-values invalid-values))))

(defn filled? [board]
  (loop [row 0]
    (cond
      (== row 9) true
      (contains? (row-values board [row 0]) 0) false
      :else (recur (inc row)))))

(defn rows [board]
  (loop [row 0
         result []]
    (if (== row 9)
      result
      (recur (inc row) (conj result (row-values board [row 0]))))))

(defn cols [board]
  (loop [col 0
         result []]
    (if (== col 9)
      result
      (recur (inc col) (conj result (col-values board [0 col]))))))

(defn blocks [board]
  (let [blocks-coords (coord-pairs [0 3 6])]
    (loop [pos 0
           result []]
      (if (== pos 9)
        result
        (recur (inc pos) (conj result (block-values board (get blocks-coords pos))))))))

(defn valid-sudoku-sets? [board sets-fetcher]
  (reduce (fn [result a-set] (if (or (not result) (contains? a-set 0))
                               false
                               (== 9 (count a-set)))) true (sets-fetcher board)))

(defn valid-rows? [board]
  (valid-sudoku-sets? board rows))

(defn valid-cols? [board]
  (valid-sudoku-sets? board cols))

(defn valid-blocks? [board]
  (valid-sudoku-sets? board blocks))

(defn valid-solution? [board]
  (and (valid-blocks? board) (valid-rows? board) (valid-cols? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [all-cords (coord-pairs (map dec all-values))]
    (loop [i 0]
      (cond
        (== i 81) nil
        (has-value? board (get all-cords i)) (recur (inc i))
        :else (get all-cords i)))))

(defn solve-helper [board solution]
  (if (filled? solution)
    (if (valid-solution? solution)
      solution
      [])
    (let [next-location (find-empty-point solution)]
      (for [option (valid-values-for solution next-location)
            sol (solve-helper board (set-value-at solution next-location option))]
        sol))))


(defn solve [board]
  (solve-helper board board))
