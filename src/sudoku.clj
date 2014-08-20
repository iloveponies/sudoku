(ns sudoku
  (:require [clojure.set :as set]))

(def all-values #{1 2 3 4 5 6 7 8 9})

(def board identity)

(board [[5 3 0 0 7 0 0 0 0]
        [6 0 0 1 9 5 0 0 0]
        [0 9 8 0 0 0 0 6 0]
        [8 0 0 0 6 0 0 0 3]
        [4 0 0 8 0 3 0 0 1]
        [7 0 0 0 2 0 0 0 6]
        [0 6 0 0 0 0 2 8 0]
        [0 0 0 4 1 9 0 0 5]
        [0 0 0 0 8 0 0 7 9]])

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (= (value-at board coord) 0) false true))

(defn row-values [board coord]
  (let [row (get board (first coord))]
    (set row)))

(defn col-values [board coord]
  (let [number (second coord)
        helper (fn [n] (get-in board (vector n number)))]
    (loop [n 0
           a-set []]
      (if (= n 9)
        (set a-set)
        (recur (inc n) (conj a-set (helper n)))))))

(defn coord-pairs [coords]
  (for [rows coords
        columns coords]
    [rows columns]))

(defn block-values [board coord]
  (let [row-x (* 3 (int (/ (first coord) 3)))
        column-y (* 3 (int (/ (second coord) 3)))
        rows (range row-x (+ row-x 3))
        columns (range column-y (+ column-y 3))
        ans []]
    (set (for [r (vec rows)
          c (vec columns)]
      (value-at board (vector r c))))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values (set/union
                                (row-values board coord)
                                (col-values board coord)
                                (block-values board coord)))))

(defn filled? [board]
  (let [values
        (for [temp (vec (range 9))]
          (row-values board [temp 0]))
        helper (reduce set/union values)]
    (if (contains? helper 0)
      false
      true)))

(defn rows [board]
  (let [values
        (for [temp (vec (range 9))]
          (row-values board [temp 0]))]
    (vec values)))

(defn valid-rows? [board]
  (let [helper (fn [x] (if (= x all-values) 1 -1))
        values (for [r (rows board)]
                  (helper r))]
    (= #{1} (set values))))

(defn cols [board]
  (let [values
        (for [temp (vec (range 9))]
          (col-values board [0 temp]))]
    (vec values)))

(defn valid-cols? [board]
  (let [helper (fn [x] (if (= x all-values) 1 -1))
        values (for [c (cols board)]
                  (helper c))]
    (= #{1} (set values))))

(defn blocks [board]
  (let [values
        (for [rows (vector 0 3 6)
              cols (vector 0 3 6)]
          (block-values board [rows cols]))]
    (vec values)))

(defn valid-blocks? [board]
  (let [helper (fn [x] (if (= x all-values) 1 -1))
        values (for [b (cols board)]
                  (helper b))]
    (= #{1} (set values))))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [rn 0
         cn 0]
    (cond
     (= rn 9) 0
     (= cn 9) (recur (inc rn) 0)
     (= 0 (value-at board [rn cn])) [rn cn]
     :else
       (recur rn (inc cn)))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board) (first [board]) [])
    (let [next-empty (find-empty-point board)
          new-value (valid-values-for board next-empty)]
      (for [temp (vec new-value)
            solution (solve (set-value-at board next-empty temp))]
        solution))))


