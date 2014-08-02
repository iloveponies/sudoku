(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board [row col]]
  (set (get board row)))

(defn col-values [board [row col]]
  (loop [results #{}
         row 0]
    (let [value (value-at board [row col])]
      (if (nil? value)
        results
        (recur (conj results value)
               (inc row))))))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-values [board coord]
  (let [top-left (map #(* 3 (int (/ % 3))) coord)
        [rows cols] (map #(set (range % (+ 3 %))) top-left)
        all-coord-pairs (set (coord-pairs (concat rows cols)))
        pairs-we-want (filter #(and (contains? rows (first %))
                                    (contains? cols (second %)))
                              all-coord-pairs)]
    (set (map #(value-at board %) pairs-we-want))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (reduce set/difference [all-values
                            (block-values board coord)
                            (col-values board coord)
                            (row-values board coord)])))

(defn filled? [board]
  (let [all-coord-pairs (coord-pairs (range 0 9))]
    (not (contains? (set (map #(has-value? board %) all-coord-pairs))
                    false))))

(defn map-value-function-over-board
  [value-function board]
  (map #(value-function board [% %]) (range 0 9)))

(defn all-sets-valid? [list-of-sets-of-values]
  (= (set list-of-sets-of-values)
     #{all-values}))

(defn validate-board-dimension
  [board dimension-fun]
  (all-sets-valid? (dimension-fun board)))

(defn rows [board]
  (map-value-function-over-board row-values board))

(defn valid-rows? [board]
  (validate-board-dimension board rows))

(defn cols [board]
  (map-value-function-over-board col-values board))

(defn valid-cols? [board]
  (validate-board-dimension board cols))

(defn blocks [board]
  (map #(block-values board %) [[0 0] [0 3] [0 6]
                                [3 0] [3 3] [3 6]
                                [6 0] [6 3] [6 6]]))

(defn valid-blocks? [board]
  (validate-board-dimension board blocks))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [all-coords (coord-pairs (range 0 9))]
    (loop [to-check (first all-coords)
           remaining (rest all-coords)]
      (if (or (not (has-value? board to-check))
              (nil? to-check))
        to-check
        (recur (first remaining) (rest remaining))))))

(defn solve-helper [board]
  (if (valid-solution? board)
    [board]
    (let [to-check (find-empty-point board)
          possible-values (valid-values-for board to-check)]
      (for [value possible-values
            solution (solve-helper (set-value-at board to-check value))]
        solution))))

(defn solve [board] (first (solve-helper board)))
