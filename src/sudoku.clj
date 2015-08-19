(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [row column]]
  (set (get board row))
  )

(defn col-values [board [row column]]
  (set (map #(get-in board [%1 column]) (range 0 9))))

(defn coord-pairs [coords]
  (into [] (for [row coords
                 column coords]
             [row column])))

(defn coord-pairs3 [[row-start col-start]]
  ;; Accepts a coordinate and returns a vector of vectors with the inner vectors representing
  ;; each of the cells within the 3 X 3 block with the argument as the top left corner cell.
  (into [] (for [row (range row-start (+ row-start 3))
                 column (range col-start (+ col-start 3))]
             [row column])))

(defn corner-coord [[row column]]
  ;; Returns a coordiate for the top left corner of the 3 x 3 block in which the 
  ;; coordinate passed as an argument exists.
  (let [corner (fn [x]
                 (* 3 (int (/ x 3))))] 
    (conj [] (corner row) (corner column))))

(defn block-values [board coord]
  (set (map #(value-at board %1) (coord-pairs3 (corner-coord coord)))))

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn valid-values-for [board coord]
  ;; Returns a set of all the values that could potential be placed in a given cell
  ;; considering the values that are already populated in the row, column, and block.
  (if (has-value? board coord)
    #{}
    (set/difference all-values 
                    (row-values board coord) 
                    (col-values board coord) 
                    (block-values board coord))))

(defn filled? [board]
  (let [all-cells (reduce #(concat %1 (coord-pairs3 %2)) 
                          [] 
                          (for [row-corners [0 3 6] 
                                col-corners [0 3 6]] 
                            [row-corners col-corners]))]
    (every? #(has-value? board %1) all-cells)))

(defn rows [board]
  (map #(row-values board [%1 0 ]) (range 0 9)) )

(defn valid-rows? [board]
  (every? #(= all-values %1) (rows board)))

(defn cols [board]
  (map #(col-values board [0 %1]) (range 0 9)) )

(defn valid-cols? [board]
  (every? #(= all-values %1) (cols board)))

(defn blocks [board]
  (let [all-corners (for [row-corners [0 3 6] 
                          col-corners [0 3 6]] 
                      [row-corners col-corners])
        cells-by-block (map coord-pairs3  all-corners)
        extract-blocks (fn [block]
                         (set (map #(value-at board %1) block)))]
    (map extract-blocks cells-by-block)))


(defn valid-blocks? [board]
  (every? #(= all-values %1) (blocks board)))

(defn valid-solution? [board]
  (and (valid-blocks? board) (valid-cols? board) (valid-rows? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [all-cells (for [row (range 0 9)
                        column (range 0 9)]
                    [row column])]
    (first (filter (fn [coord] (zero? (value-at board coord))) all-cells))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      '())
    (for [empty-cell [(find-empty-point board)]
          possibility (valid-values-for board empty-cell)
          solution (solve (set-value-at board empty-cell possibility))]
      solution
      )
    ))

