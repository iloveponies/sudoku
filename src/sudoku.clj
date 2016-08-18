(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board [(first coord) (second coord)]))

(defn has-value? [board coord]
  (not (= (value-at board coord) 0)))

(defn row-values [board coord]
  (let [row (first coord)]
    (loop [acc 0
           theSet #{}]
      (if (= acc 9)
        theSet
        (recur (inc acc) (conj theSet (get-in board [row acc])))))))

(defn col-values [board coord]
  (let [col (second coord)]
    (loop [acc 0
           theSet #{}]
      (if (= acc 9)
        theSet
        (recur (inc acc) (conj theSet (get-in board [acc col])))))))

(defn col-values-seq [board coord]
  (let [col (second coord)]
    (loop [acc 0
           theSet []]
      (if (= acc 9)
        theSet
        (recur (inc acc) (conj theSet (get-in board [acc col])))))))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    (vector x y)))

(defn coord-pairs-two [coords1 coords2]
  (for [x coords1
        y coords2]
    (vector x y)))

(defn block-values-helper [board coord]
  (let [rows (first coord)
        cols (second coord)]
    (cond
      (<= 0 rows 2)
        (cond
          (<= 0 cols 2)(for [x (coord-pairs-two [0 1 2] [0 1 2])] (value-at board x))
          (<= 3 cols 5)(for [x (coord-pairs-two [0 1 2] [3 4 5])] (value-at board x))
          (<= 6 cols 8)(for [x (coord-pairs-two [0 1 2] [6 7 8])] (value-at board x)))
      (<= 3 rows 5)
        (cond
          (<= 0 cols 2)(for [x (coord-pairs-two [3 4 5] [0 1 2])] (value-at board x))
          (<= 3 cols 5)(for [x (coord-pairs-two [3 4 5] [3 4 5])] (value-at board x))
          (<= 6 cols 8)(for [x (coord-pairs-two [3 4 5] [6 7 8])] (value-at board x)))
      (<= 6 rows 8)
        (cond
          (<= 0 cols 2)(for [x (coord-pairs-two [6 7 8] [0 1 2])] (value-at board x))
          (<= 3 cols 5)(for [x (coord-pairs-two [6 7 8] [3 4 5])] (value-at board x))
          (<= 6 cols 8)(for [x (coord-pairs-two [6 7 8] [6 7 8])] (value-at board x))))))

(defn block-values [board coord]
  (set (block-values-helper board coord)))

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn valid-values-for [board coord]
  (if (not (= 0 (value-at board coord)))
    #{}
    (let [valid-block (set/difference all-values (block-values board coord))
          valid-row (set/difference all-values (row-values board coord))
          valid-col (set/difference all-values (col-values board coord))]
       (set/intersection valid-block valid-row valid-col))))

(defn filled? [board]
  (let [set-of-values (set (apply concat board))]
    (not (contains? set-of-values 0))))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (cond
    (empty? board) true
    (contains? (set (first board)) 0) false
    :else (let [freqs (filter (fn [x] (not (= x 1)))
                        (map second (frequencies (first board))))]

      (if (not (empty? freqs))
       false
      (valid-rows? (rest board))))))

(defn cols [board]
  (loop [acc 0
         set-of-cols []]
    (if (= acc 9)
        set-of-cols
      (recur (inc acc) (conj set-of-cols (col-values board [0 acc]))))))

(defn valid-cols? [board]
  (loop [acc 0]
    (cond
      (= acc 9) true
      (contains? (set (col-values board [0 acc])) 0) false
      :else (let [freqs (filter (fn [x] (> x 1))
                                (map second (frequencies (col-values-seq board [0 acc]))))]
              (if (not (empty? freqs))
                false
                (recur (inc acc)))))))

(defn blocks-helper-cols [board row]
  (loop [col 0
         set-of-blocks []]
    (if (> col 8)
      set-of-blocks
      (recur (+ col 3) (conj set-of-blocks (block-values-helper board [row col]))))))

(defn blocks-helper-rows [board]
  (loop [row 0
         set-of-blocks []]
    (if (> row 8)
      set-of-blocks
      (recur (+ row 3) (conj set-of-blocks (blocks-helper-cols board row))))))

(defn blocks [board]
  (map set (apply concat (blocks-helper-rows board))))

(defn valid-blocks? [board]
  (loop [block-values-set (apply concat (blocks-helper-rows board))]
    (cond
      (contains? (set (first block-values-set)) 0) false
      (empty? block-values-set) true
      (not (empty? (filter (fn [x] (> x 1))
                           (map second (frequencies (first block-values-set)))))) false
      :else (recur (rest block-values-set)))))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [cols 0
         rows 0]
    (if (= 0 (value-at board [rows cols]))
      [rows cols]
      (if (= cols 9)
        (if (= rows 9)
          nil
          (recur 0 (inc rows)))
        (recur (inc cols) rows)))))

(defn solve-helper [board]
  (if (valid-solution? board)
    [board]
    (let [point (find-empty-point board)]
      (for [valid (valid-values-for board point)
            solution (solve-helper (set-value-at board point valid))]
        solution))))

(defn solve [board]
  (first (solve-helper board)))
