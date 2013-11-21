(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (= 0 (value-at board coord))
    false
    true))

(defn row-values [board coord]
  (let [[x y] coord]
    (set (sort (distinct (get board x))))
    ))

(defn col-values [board coord]
  (let [[x y] coord]
    (loop [n   0
           ret []]
      (if (= n (count board))
        (set ret)
        (recur (inc n)
               (conj ret (value-at board [n y])))))
    ))

(defn coord-pairs [coords]
   (for [x coords
         y coords]
     [x y]))

(defn upper-left [x y]
  (vector (- x (mod x 3))
          (- y (mod y 3))))

(defn block-values [board coord]
  (let [[x y] coord
        upper (upper-left x y)
        [a b] upper]
    (set (sort (distinct (flatten (conj
    (take 3 (drop b (get board a)))
    (take 3 (drop b (get board (+ 1 a))))
    (take 3 (drop b (get board (+ 2 a))))
    )))))))



(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set (filter (fn [x] (not (or
                    (contains? (row-values board coord) x)
                    (contains? (col-values board coord) x)
                    (contains? (block-values board coord) x))))
            all-values))))


(defn filled? [board]
  (loop [n 0
         bool true]
    (if (or (not bool) (= 9 n))
      bool
  (recur
   (inc n)
   (if (contains? (row-values board [n 0]) 0)
          false
          true)))))

(defn rows [board]
  (loop [n 0
         final []]
    (if (= n 9)
      final
      (recur (inc n)
             (conj final (row-values board [n 0]))))))

(defn valid-rows? [board]
  (loop [n 0
         bool true]
    (cond
     (not bool) bool
     (= 9 n) bool
     :else (recur (inc n)
                  (= all-values (row-values board [n 0])))
     )))

(defn cols [board]
    (loop [n 0
         final []]
    (if (= n 9)
      final
      (recur (inc n)
             (conj final (col-values board [0 n]))))))

(defn valid-cols? [board]
  (loop [n 0
         bool true]
    (cond
     (not bool) bool
     (= 9 n) bool
     :else (recur (inc n)
                  (= all-values (col-values board [0 n])))
     )))

(defn blocks [board]
  (loop [n 0
         final []]
    (if (= n 3)
      final
      (recur (inc n)
             (conj final (block-values board [(* 3 n) 0])
                         (block-values board [(* 3 n) 3])
                         (block-values board [(* 3 n) 6]))))))

(defn valid-blocks? [board]
  (loop [n 0
         bool true]
    (cond
     (not bool) bool
     (= 3 n) bool
     :else (recur (inc n)
                  (and (= all-values (block-values board [(* 3 n) 0]))
                       (= all-values (block-values board [(* 3 n) 3]))
                       (= all-values (block-values board [(* 3 n) 6])))
     ))))

(defn valid-solution? [board]
  (and (valid-blocks? board)
       (valid-cols? board)
       (valid-rows? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [x 0
         y 0]
    (cond
     (= 9 x) []
     (= 0 (value-at board [x y])) [x y]
     :else(recur (if (= 9 y)
                   (inc x)
                   x)
                 (if (= 9 y)
                   (- y 9)
                   (inc y)))
    )))


(defn solve-helper [board current]
  (if (filled? current)
    (if (valid-solution? current)
    current
      ())
    (let [loc (find-empty-point current)]
      (for [val (valid-values-for current loc)
            solution (solve-helper board
                                   (set-value-at current loc val))]
        solution)
      )))

(defn solve [board]
  (solve-helper board board)
    )

