(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= (value-at board coord) 0)))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (let [[x y] coord]
    (loop [i 0
           values #{}]
      (if (= i 9)
        values
        (recur (inc i)
               (conj values (value-at board
                                    [i y])))))))
(defn coord-pairs [coords]
  (apply vector (for [x coords
                      y coords]
                  [x y])))

(defn block-values [board coord]
  (let [top-coordinates (fn self [coords]
                          (let [[x y] coords]
                            (if (= (mod x 3) 0)
                              (if (= (mod y 3) 0)
                                coords
                                (self [x (- y 1)]))
                              (self [(- x 1) y]))))
        x1 (first (top-coordinates coord))
        y1 (second (top-coordinates coord))]
    (set (for [x (range x1 (+ x1 3))
          y (range y1 (+ y1 3))]
      (value-at board [x y])))))

(defn valid-values-for [board coord]
  (if (> (value-at board coord) 0) #{}
    (set/difference #{1 2 3 4 5 6 7 8 9}
                    (set/union (block-values board coord)
                               (set/union (row-values board coord)
                                          (col-values board coord))))))

(defn filled? [board]
  (let [all-values (fn [board]
                     (reduce concat
                             []
                             board))]
    (not (some zero? (all-values board)))))

(defn rows [board]
  (reduce (fn [init x]
            (conj init (set x)))
          []
          board))


(defn valid-rows? [board]
  (every? (fn [x]
            (= (set x) #{1 2 3 4 5 6 7 8 9}))
          board))

(defn cols [board]
  (reduce (fn [init x]
            (conj init (col-values board [0 x])))
            []
            (apply vector (range 9))))

(defn valid-cols? [board]
  (every? (fn [x]
            (= #{1 2 3 4 5 6 7 8 9} x))
          (cols board)))

(defn blocks [board]
  (reduce (fn [init x]
            (conj init (block-values board x)))
          []
          (for [x [0 3 6]
                y [0 3 6]]
            [x y])))

(defn valid-blocks? [board]
  (every? (fn [x]
            (= #{1 2 3 4 5 6 7 8 9} x))
          (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [traverse (fn self [y x]
                   (cond (and (= y 9) (= x 9)) nil
                         (= x 9) (self (+ y 1) 0)
                         :else (if (= (value-at board [y x]) 0)
                                 [y x]
                                 (self y (+ x 1)))))]
    (traverse 0 0)))

(defn solve [board]
  (defn all-solutions [board]
    (let [empty-point (find-empty-point board)]
      (if (nil? empty-point)
        (if (valid-solution? board)
          [board]
          [])
        (for [guess (valid-values-for board empty-point)
              solution (all-solutions (set-value-at board empty-point guess))]
          solution))))

  (first (take 1 (all-solutions board))))
