(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(def crazy-board
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

(def finished-board
  (board [[5 3 4 6 7 8 9 1 2]
          [6 7 2 1 9 5 3 4 8]
          [1 9 8 3 4 2 5 6 7]
          [8 5 9 7 6 1 4 2 3]
          [4 2 6 8 5 3 7 9 1]
          [7 1 3 9 2 4 8 5 6]
          [9 6 1 5 3 7 2 8 4]
          [2 8 7 4 1 9 6 3 5]
          [3 4 5 2 8 6 1 7 9]]))

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (zero? (value-at board coord))
    false
    true))

(defn row-values [board coord]
  (set (get board (first coord))))

; (defn col-values [board coord]
;   (let [col-number (second coord)]
;     (set (map #(get %1 col-number) board))))

(defn col-values [board coord]
  (let [col-number (second coord)]
    (reduce #(conj %1 (get %2 col-number)) #{} board)))

(defn coord-pairs [coords]
  (for [ca coords
        cb coords]
    [ca cb]))

(defn first-of-block [coord]
  (let [helper (fn [n]
                 (cond
                   (>= n 6) 6
                   (>= n 3) 3
                   :else 0))]
    (map helper coord)))

(defn block-values [board coord]
  (let [[row col] (first-of-block coord)
        range-col (range col (+ 3 col))
        range-row (range row (+ 3 row))]

    (set (for [r range-row
               c range-col]

           (value-at board [r c])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [values (set/union (block-values board coord)
                            (col-values board coord)
                            (row-values board coord))]
      (set/difference all-values values))))

(defn filled? [board]
  (if (empty? board)
    true
    (if (.contains (first board) 0)
      false
      (filled? (rest board)))))

(defn rows [board]
  (map #(set %1) board))

(defn valid-stuff? [stuff board]
  (let [stuff (stuff board)]
    (loop [stuff stuff
           items (first stuff)]
      (if (empty? stuff)
        true
        (if (not= 9 (count items))
          false
          (recur (rest stuff) (first (rest stuff))))))))

(def valid-rows?
  (partial valid-stuff? rows))

(defn cols [board]
  (let [cols (range 0 9)]
    (map #(col-values board [0 %1]) cols)))

(def valid-cols?
  (partial valid-stuff? cols))

(defn blocks [board]
  (let [opts [0 3 6]]
    (for [r opts
          c opts]
      (block-values board [r c]))))

(def valid-blocks?
  (partial valid-stuff? blocks))

(defn valid-solution? [board]
  (let [funks [valid-rows? valid-cols? valid-blocks?]]
    (reduce #(and %1 %2) (map #(%1 board) funks))))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [row 0, board board]
    (let [f (first board)
          ff (if (nil? f) '() f)
          col (.indexOf ff 0)]
      (cond
        (empty? board) nil
        (not= -1 col) [row col]
        :else (recur (inc row) (rest board))))))

(def counter (atom 0))

(defn solve-helper [board]
  (swap! counter inc)
  (if (filled? board)
    (if (valid-solution? board)
      [board]
      [])
    (do
      (let [empty (find-empty-point board)
            valid-values (valid-values-for board empty)]
        (for [value valid-values
              solution (solve-helper (set-value-at board empty value))]
          solution)))))

(defn solve [board] nil
  (first (solve-helper board)))

(defn -main []
  (solve crazy-board)
  (println (str "number of function calls: " @counter)))
