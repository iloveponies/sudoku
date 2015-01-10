(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (set (map (fn [row] (get row (second coord))) board)))

(defn coord-pairs [coords]
  (loop [pairs []
         position 0
         keyval (get coords position) ]
    (if (< position (count coords))
      (let [next-position (inc position)]
        (recur
          (concat pairs
                  (for [x coords]
                    (vector keyval x)))
          (inc position)
          (get coords (inc position))))
      pairs)))

(defn top-left [coord]
  (let [helper (fn [x] (* 3 (int (/ x 3))))]
    [(helper (first coord)) (helper (second coord))]))
(defn block-values [board coord]
  (let [tl (top-left coord)
        block-coords (map (fn [c]
                            (let [[x y] c]
                              [(+ x (first tl)) (+ y (second tl))]))
                          (coord-pairs [0 1 2]))]
    (set (map (fn [c] (value-at board c)) block-coords))))

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn valid-values-for [board coord]
  (let [square-values (block-values board coord)]
    (if (has-value? board coord)
      #{}
      (set/difference all-values
                      square-values
                      (set (row-values board coord))
                      (set (col-values board coord))))))

(defn filled? [board]
  (empty? (filter zero? (reduce concat [] board))))

(defn valid-helper [a-seq]
  (empty? (filter (fn [x]
                    (not (empty? (set/difference all-values x))))
                  a-seq)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (valid-helper (rows board)))

(defn cols [board]
  (map (fn [x] (col-values board [0 x])) (range 0 (count board))))

(defn valid-cols? [board]
  (valid-helper (cols board)))

(defn blocks [board]
  (map (fn [x]
         (block-values board x))
       (let [top-left-corner [0 3 6]]
         (for [x top-left-corner
               y top-left-corner]
           [x y]))))

(defn valid-blocks? [board]
  (valid-helper (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-cols? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-first-index [pred a-seq]
  (loop [n 0
         aa-seq a-seq]
    (cond
      (empty? aa-seq) nil
      (pred (first aa-seq)) n
      :else (recur (inc n) (rest aa-seq)))))
(defn find-empty-point [board]
  (let [n (count board)
        pos (find-first-index zero? (reduce concat board))]
    [(int (/ pos n)) (mod pos n)]))

(defn solve [board]
  (if (valid-solution? board)
    board
    (let [coord (find-empty-point board)
          new-values (valid-values-for board coord)]
      (for [new-value new-values
            solution (solve (set-value-at board coord new-value))]
        solution))))
