(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (int (get-in board coord)))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (let [row (get coord 0)]
    (set (get board row)))
  )

(defn col-values [board coord]
  (let [col (get coord 1)]
    (set (for [row (range 9)]
        (get-in board [row col])))))

(defn coord-pairs [coords]
  (cond
    (empty? coords) nil
    (= 2 (count coords))
      (for [x1 (vector (get coords 0))
            x2 (vector (get coords 1))]
        (vector x1 x2))
    :else (for [x1 (vector (get coords 0))
                x2 (vector (get coords 1))]
            (vector (vector x1 x2) (coord-pairs (vector x2 (rest coords)))))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
        (vector row col)))


(defn block-values [board coord]
  (let [top-left (fn [c] (cond
                            (<= 0 c 2) 0
                            (<= 3 c 5) 3
                            (<= 6 c 9) 6))
        x (get coord 0)
        y (get coord 1)
        top-left-x (top-left x)
        top-left-y (top-left y)]
    (set (for [row (range top-left-x (+ top-left-x 3))
               col (range top-left-y (+ top-left-y 3))]
           (value-at board [row col])))))

(defn valid-values-for [board coord]
  (if (not (zero? (value-at board coord)))
    #{}
  (let [used-block-values (block-values board coord)
        used-row-values (row-values board coord)
        used-col-values (col-values board coord)
        used-values (clojure.set/union used-block-values used-row-values used-col-values)]
    (set/difference all-values used-values))))

(defn filled? [board]
  (not (contains? (set (for [row (range 9)
             col (range 9)]
         (value-at board [row col]))) 0)))

(defn rows [board]
  (for [row (range 9)]
            (set (row-values board [row 0]))))

(defn cols [board]
  (for [col (range 9)]
            (set (col-values board [0 col]))))

(defn blocks [board]
  (reduce conj []  (for [row (range 0 9 3)
                col (range 0 9 3)]
            (block-values board [row col]))))

(defn valid-rows? [board]
  (reduce (fn [a b] (and a (= b all-values)))
              true (rows board)))

(defn valid-cols? [board]
  (reduce (fn [a b] (and a(= b all-values)))
              true (cols board)))

(defn valid-blocks? [board]
  (reduce (fn [a b] (and a (= b all-values)))
              true (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [row 0
         col 0]
    (let [atval (value-at board [row col])]
      (cond
        (zero? atval) (vector row col)
        (and (= 8 row) (= 8 col)) nil
        (= 8 col) (recur (inc row) 0 )
        :else (recur row (inc col))))))

(defn solve [board]
  (let [b board
        ep (find-empty-point b)]
      (cond
        (valid-solution? b) b
        (nil? ep) ()
        :else (for [v all-values]
                (solve (set-value-at b ep v))))))

