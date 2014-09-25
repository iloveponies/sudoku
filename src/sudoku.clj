(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(def sudoku-board
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

(def solved-board
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

(value-at sudoku-board [0 1]) ;=> 3
(value-at sudoku-board [0 0]) ;=> 5

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(has-value? sudoku-board [0 0]) ;=> true
(has-value? sudoku-board [0 2]) ;=> false

(defn row-values [board [r c]]
  (set (get board r)))

(row-values sudoku-board [0 2]) ;=> #{0 5 3 7}
(row-values sudoku-board [3 2]) ;=> #{0 8 6 3}

(defn col-values [board [r c]]
  (set (map (fn [row] (get row c)) board)))

(col-values sudoku-board [0 2]) ;=> #{0 8}
(col-values sudoku-board [4 8]) ;=> #{3 1 6 0 5 9}

(defn coord-pairs [coords]
  (vec (for [x coords
             y coords]
         [x y]
         )))



(coord-pairs [0 1])   ;=> [[0 0] [0 1]
                      ;    [1 0] [1 1]]

(coord-pairs [0 1 2]) ;=> [[0 0] [0 1] [0 2]
                      ;    [1 0] [1 1] [1 2]
                      ;    [2 0] [2 1] [2 2]]

(coord-pairs [3 4 5]) ;=> [[0 0] [0 1] [0 2]
                      ;    [1 0] [1 1] [1 2]
                      ;    [2 0] [2 1] [2 2]]                   
     
(defn block-topleft [[r c]]
  [(* 3 (int (/ r 3))) (* 3 (int (/ c 3)))])
  
(block-topleft [0 0])
(block-topleft [1 2])
(block-topleft [3 8])

(defn block-values [board [r c]]
 (let [[top left] (block-topleft [r c])
       values (for [row (range top (+ top 3))
                    col (range left (+ left 3))]
                (value-at board [row col]))]
   (set values)))

(block-values sudoku-board [0 0]) ;=> #{0 5 3 6 8 9}
(block-values sudoku-board [0 2]) ;=> #{0 5 3 6 8 9}
(block-values sudoku-board [4 5]) ;=> #{0 6 8 3 2}


(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values 
                    (set/union (block-values board coord) 
                               (row-values board coord) 
                               (col-values board coord)))))

(valid-values-for sudoku-board [0 0]) ;=> #{}
(valid-values-for sudoku-board [0 2]) ;=> #{1 2 4})

(defn filled? [board]
  (every? (fn [coord] (has-value? board coord)) (coord-pairs (range 0 9))))

(filled? sudoku-board) ;=> false
(filled? solved-board) ;=> true


(defn rows [board]
  (vec (map (fn [row] (set row)) board)))

(rows sudoku-board) ;=> [#{5 3 0 7}
                    ;    #{6 0 1 9 5}
                    ;    #{0 9 8 6}
                    ;    #{8 0 6 3}
                    ;    #{4 0 8 3 1}
                    ;    #{7 0 2 6}
                    ;    #{0 6 2 8}
                    ;    #{0 4 1 9 5}
                    ;    #{0 8 7 9}]

(rows solved-board) ;=> [#{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}]


(defn valid-rows? [board]
  (every? (fn [row] (= all-values (row-values board [row 0]))) (range 0 9)))

(valid-rows? solved-board)  ;=> truthy
(valid-rows? sudoku-board) ;=> falsey

(defn cols [board]
  (vec (for [col (range 0 9)]
         (set (map (fn [row] (get row col)) board)))))
         

(cols sudoku-board) ;=> [#{5 6 0 8 4 7}
                    ;    #{3 0 9 6}
                    ;    #{0 8}
                    ;    #{0 1 8 4}
                    ;    #{7 9 0 6 2 1 8}
                    ;    #{0 5 3 9}
                    ;    #{0 2}
                    ;    #{0 6 8 7}
                    ;    #{0 3 1 6 5 9}]

(cols solved-board) ;=> [#{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}
                    ;    #{1 2 3 4 5 6 7 8 9}]

(defn valid-cols? [board]
  (every? (fn [col] (= all-values (col-values board [0 col]))) (range 0 9)))

(valid-cols? solved-board)  ;=> truthy
(valid-cols? sudoku-board) ;=> falsey

(defn blocks [board]
  (vec (for [col [0 3 6]
             row [0 3 6]]
         (block-values board [col row]))))

(blocks sudoku-board) ;=> [#{5 3 0 6 9 8}
                      ;    #{0 7 1 9 5}
                      ;    #{0 6}
                      ;    #{8 0 4 7}
                      ;    #{0 6 8 3 2}
                      ;    #{0 3 1 6}
                      ;    #{0 6}
                      ;    #{0 4 1 9 8}
                      ;    #{2 8 0 5 7 9}]

(blocks solved-board) ;=> [#{1 2 3 4 5 6 7 8 9}
                      ;    #{1 2 3 4 5 6 7 8 9}
                      ;    #{1 2 3 4 5 6 7 8 9}
                      ;    #{1 2 3 4 5 6 7 8 9}
                      ;    #{1 2 3 4 5 6 7 8 9}
                      ;    #{1 2 3 4 5 6 7 8 9}
                      ;    #{1 2 3 4 5 6 7 8 9}
                      ;    #{1 2 3 4 5 6 7 8 9}
                      ;    #{1 2 3 4 5 6 7 8 9}])


(defn valid-blocks? [board]
  (let [toplefts (for [row [0 3 6] 
                       col [0 3 6]]
                   [row col])]
  (every? (fn [coord] (= all-values (block-values board coord))) toplefts)))

(valid-blocks? solved-board)  ;=> truthy
(valid-blocks? sudoku-board) ;=> falsey

(defn valid-solution? [board]
  (and (valid-cols? board) (valid-rows? board) (valid-blocks? board)))

(valid-solution? solved-board)  ;=> truthy
(valid-solution? sudoku-board) ;=> falsey)
  
(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(def before-change
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 9 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

(def after-change
  (board [[5 3 0 0 7 0 0 0 0]
          [6 0 0 1 9 5 0 0 0]
          [0 4 8 0 0 0 0 6 0]
          [8 0 0 0 6 0 0 0 3]
          [4 0 0 8 0 3 0 0 1]
          [7 0 0 0 2 0 0 0 6]
          [0 6 0 0 0 0 2 8 0]
          [0 0 0 4 1 9 0 0 5]
          [0 0 0 0 8 0 0 7 9]]))

(set-value-at before-change [2 1] 4)


(defn find-empty-point [board]
  (let [coords (for [row (range 0 9) 
                     col (range 0 9)]
                   [row col])]
    (some (fn [coord] (if (has-value? board coord)
                        false
                        coord)) coords))) 


(find-empty-point sudoku-board)

(defn solve [board]
  nil)
