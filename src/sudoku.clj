(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn is-value? [x]
  (not= 0 x))

(defn has-value? [board coord]
  (is-value? (value-at board coord)))

(defn row-values [board coord]
  (let [[row _] coord]
    (set (get board row))))

(defn col-values2 [board coord]
  (let [[_ col] coord]
    (set
      (for [row board]
        (get row col)))))

(defn col-values [board coord]
  (let [[_ col] coord]
    (set
      (map first
           (map (partial drop col) board)))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-top-coords [coords]
  (let [[row col] coords]
    [(- row (mod row 3)) (- col (mod col 3))]))

(defn block-values [board coord]
  (let [[block-y block-x] (block-top-coords coord)]
    (set
      (for [row (range block-y (+ 3 block-y))
            col (range block-x (+ 3 block-x))]
        (value-at board [row col])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
                    (row-values board coord)
                    (col-values board coord)
                    (block-values board coord))))

(defn board->seq [board]
  (apply concat board))

(defn filled? [board]
  (not (contains? (set (board->seq board)) 0)))

(defn rows [board]
  (for [row board]
    (set row)))

(defn valid-rows? [board]
  (every? #(= %1 all-values) (rows board)))

(defn cols3 [board]
  (for [i (range 0 9)]
    (set
      (for [row board]
        (nth row i)))))

(defn print-board [board]
  (for [row board]
    (println row)))

(defn cols2 [board]
  (loop [board board
         result []]
    (if (some empty? board)
      result
      (recur (map rest board) (conj result (set (map first board)))))))

(defn cols [board]
  (loop [board board
         result []]
    (if (some empty? board)
      result
      (recur (map rest board) (conj result (set (map first board)))))))

(defn valid-cols? [board]
  (every? #(= %1 all-values) (cols board)))

;(defn blocks2 [board]
;  (loop [board-seq (board->seq board)
;         i 0
;         result (take 9 (repeat #{}))]
;    (if (empty? board-seq)
;      result
;      (let [bx (mod i 3)
;            by (mod i (* 3 3))
;            block (+ bx (* 3 by))]
;        (recur
;          (rest board-seq)
;          (inc i)
;          (conj (nth result block)
;                5))))))

;(defn blocks [board]
;  (loop [board-seq (board->seq board)
;         i 0
;         x 0
;         y 0
;         result []
;         block #{}]
;    (if (empty? board-seq)
;      result
;      (recur (rest board-seq)
;             (inc i)
;             (mod i 9)
;             (int (/ y 9))
;             result
;             (conj block (first board-seq))))))
;(defn blocks [board]
;  (for [y (range 0 9)
;        x (range 0 9)]
;    (let [block (+ (mod x 3) (* 3 (mod y 3)))]
;      (println "x" x "y" y " block" block)
;      )))


(defn block [board-seq coords]
  (let [[by bx] coords
        start-point (drop (+ (* by 27) (* bx 3)) board-seq)]
    (set
      (apply concat
             (take 3 (partition 3 9 start-point))))))

(defn blocks [board]
  (let [board-seq (board->seq board)]
    (for [by (range 0 3)
          bx (range 0 3)]
      (block board-seq [by bx]))))

(defn blocks2 [board]
  (let [board-seq (board->seq board)]
    (for [i (range 0 9)]
      (set
        (apply
          concat
          (take 3 (partition 3 9 (drop (+ (* 3 (mod i 3)) (* 27 (int (/ i 3)))) board-seq))))))))

(defn blocks-ei-toimi [board]
  (for [i (range 0 9 3)]
    (for [a (map (partial partition 3 9) (partition (* 3 9) (drop i (board->seq board))))]
      (apply set/union (map set a)))))

(defn valid-blocks? [board]
  (every? #(= %1 all-values) (blocks board)))

(defn valid-solution? [board]
  (and
    (valid-rows? board)
    (valid-cols? board)
    (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [board-seq (board->seq board)
         i 0]
    (let [x (mod i 9)
          y (int (/ i 9))]
      (cond
        (empty? board-seq) nil
        (= 0 (first board-seq)) [y x]
        :else (recur (rest board-seq) (inc i))))))

(defn solve [board]
  (if (nil? (find-empty-point board))
    (if (valid-solution? board)
      board
      nil)
    (let [empty-point (find-empty-point board)
          valid-values (valid-values-for board empty-point)]
      (let [res (for [value valid-values]
                  (solve (set-value-at board empty-point value)))]
        (if (empty? res)
          []
          (first (filter not-empty res)))))))

