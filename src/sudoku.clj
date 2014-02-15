(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board coord]
  (let [row (first coord)] 
    (loop [col 0
           acc #{}]
      (if (= 9 col)
        acc
        (recur (inc col) 
               (conj acc (value-at board [row col])))))))

(defn col-values [board coord]
  (let [col (get coord 1)] 
    (loop [row 0
           acc #{}]
      (if (= 9 row)
        acc
        (recur (inc row) 
               (conj acc (value-at board [row col])))))))

(defn coord-pairs 
  ([coords]
    (for [row coords
          col coords]
      [row col]))
  ([top-row left-col n]
    (for [x (range n)
          y (range n)]
      [(+ top-row x) (+ left-col y)])))

(defn- block-beginning [row-or-col]
  (* 3 (int (/ row-or-col 3))))

(defn- block-upper-left-corner [coord]
  [(block-beginning (first coord)) (block-beginning (get coord 1))])

(defn block-values [board coord]
  (let [[block-top block-left] (block-upper-left-corner coord)]
    (reduce (fn [acc c] (conj acc (value-at board c)))
            #{} 
            (coord-pairs block-top block-left 3))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values 
                    (row-values board coord) 
                    (col-values board coord) 
                    (block-values board coord))))

(defn filled? [board]
  (let [board-as-seq (for [x (range 1 10)
                           y (range 1 10)]
                       (value-at board [x y]))]
    (not (contains? (set board-as-seq) 0))))

(defn rows [board]
  (for [row (range 0 9)]
    (row-values board [row 0])))

(defn- all-valid? [row-col-or-block]
  (every? (fn [s] (empty? (set/difference all-values s))) row-col-or-block))

(defn valid-rows? [board]
  (all-valid? (rows board)))

(defn cols [board]
  (for [col (range 0 9)]
    (col-values board [0 col])))

(defn valid-cols? [board]
  (all-valid? (cols board)))

(defn blocks [board]
  (for [x (range 0 3)
        y (range 0 3)]
    (block-values board [(* 3 x) (* 3 y)])))

(defn valid-blocks? [board]
  (all-valid? (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [x 0
         y 0]
    (cond 
      (>= x 9)
      nil
      (zero? (value-at board [x y]))
      [x y]
      :else
      (let [next-x (if (= y 8) (inc x) x)
            next-y (if (= y 8) 0 (inc y))]
      (recur next-x next-y)))))

(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      nil)
    (let [ep (find-empty-point board)
          valid-values-here (valid-values-for board ep)]
      (loop [values-left-to-try valid-values-here]
        (if (empty? values-left-to-try)
          nil
          (let [potential-solution (solve (set-value-at board ep (first values-left-to-try)))]
            (if (not (nil? potential-solution))
              potential-solution
              (recur (rest values-left-to-try)))))))))

(def sudoku-board
  (board [[2 0 5 4 0 3 0 0 8]
          [0 0 0 0 5 9 0 0 0]
          [0 1 0 7 0 0 0 3 0]
          [1 0 0 0 0 4 0 9 6]
          [9 0 4 3 8 6 1 0 7]
          [6 3 0 9 0 0 0 0 4]
          [0 7 0 0 0 1 0 4 0]
          [0 0 0 6 9 0 0 0 0]
          [3 0 0 2 0 7 8 0 1]]))