(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
    (get-in board coord))

(defn has-value? [board coord]
    (> (get-in board coord) 0))

(defn row-values [board coord]
    (set (get board (first coord))))

(defn col-values [board coord]
    (let [indexes (range (second coord) 81 9)
          v-board (reduce concat board)]
        (set (map (fn [x] (nth v-board x)) indexes))))

(defn coord-pairs [coords]
    (for [i coords
          j coords]
        [i j]))

(defn block-values [board coord]
    (let [[x y] coord
          xb (* (int (/ x 3)) 3)
          yb (* (int (/ y 3)) 3)
          coords (for [i (range xb (+ xb 3))
                       j (range yb (+ yb 3))]
                     [i j])]
        (set (for [i coords] (get-in board i)))))

(defn valid-values-for [board coord]
    (if (has-value? board coord)
        #{}
        (clojure.set/difference all-values 
                        (clojure.set/union (row-values board coord)
                                   (col-values board coord)
                                   (block-values board coord)))))

(defn filled? [board]
    (every? true?
        (for [i (range 0 9)
              j (range 0 9)]
            (has-value? board [i j]))))

(defn rows [board]
    (for [i (range 0 9)]
        (row-values board [i 0])))

(defn valid-rows? [board]
    (every? true?
        (for [i (range 0 9)]
            (= (set (row-values board [i 0])) all-values))))

(defn cols [board]
    (for [i (range 0 9)]
        (col-values board [0 i])))

(defn valid-cols? [board]
    (every? true? (for [i (range 0 9)]
                      (= (set (col-values board [0 i])) all-values))))

(defn blocks [board]
    (for [i (range 0 9 3)
          j (range 0 9 3)]
        (block-values board [i j])))

(defn valid-blocks? [board]
    (every? true?
        (for [i (range 0 9 3)
              j (range 0 9 3)]
            (= (set (block-values board [i j])) all-values) )))

(defn valid-solution? [board]
    (and
        (valid-rows? board)
        (valid-cols? board)
        (valid-blocks? board)))

(defn set-value-at [board coord new-value]
    (assoc-in board coord new-value))

(defn find-empty-point [board]
    (loop [i 0
           j 0]
        (cond
            (and (= i 9))                         nil
            ((complement has-value?) board [i j]) [i j]
            :else                                 (recur (if (= j 8) (inc i) i) (mod (inc j) 9)))))

(defn sweep [board]
    (loop [b board
           i 0
           j 0]
        (if (and (= i 9))
            b
            (let [next-i (if (= j 8) (inc i) i)
                  next-j (mod (inc j) 9)
                  valid-values (valid-values-for b [i j])]
                (if (= (count valid-values) 1)
                    (recur (set-value-at b [i j] (first valid-values)) next-i next-j)
                    (recur b next-i next-j))))))

(defn solve-logically [board]
    (let [next-board (sweep board)]
        (if (= board next-board)
            board
            (recur next-board))))

(defn find-first [f a-seq]
    (first (filter f a-seq)))

(defn stall? [board]
    ((complement nil?) (some (fn [x] (and ((complement has-value?) board x) 
                                          (empty? (valid-values-for board x))))
        (for [i (range 0 9)
              j (range 0 9)]
            [i j]))))

(defn solve [board]
    (let [next-step (solve-logically board)]
        (cond
            (filled? next-step) next-step
            (stall? next-step)  []
            :else 
                (let [empty-point (find-empty-point next-step)]
                    (find-first (fn [x] (valid-solution? x))
                                (for [i (valid-values-for next-step empty-point)]
                                    (solve (set-value-at next-step empty-point i))))))))
