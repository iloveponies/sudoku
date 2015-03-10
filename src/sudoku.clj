(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board [row _]]
  (set (get board row)))

(defn col-values [board [_ col]]
  (reduce (fn [acc-set row] (conj acc-set (get row col))) #{} board))

(defn coord-pairs [coord]
  (for [x coord
        y coord]
    [x y]))

(defn block-values [board coord]
  (defn top-left [coord]
    (map (fn [x] (cond (and (>= x 0) (<= x 2)) 0
                       (and (>= x 3) (<= x 5)) 3
                       (and (>= x 6) (<= x 8)) 6)) coord))
  (let [tl (top-left coord)]
    (set
     (for [x (range (first tl) (+ (first tl) 3))
           y (range (second tl) (+ (second tl) 3))]
       (value-at board [x y])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (clojure.set/difference #{0 1 2 3 4 5 6 7 8 9}
                            (row-values board coord)
                            (col-values board coord)
                            (block-values board coord))))

(defn filled? [board]
  (every? false?
          (map (fn [x] (contains? x 0))
               (map (fn [r] (row-values board [r 0]))
                    (range 0 9)))))

(defn rows [board]
  (vec
   (map (fn [r] (row-values board [r 0])) (range 0 9))))

(defn valid-rows? [board]
  (every? true?
          (map (fn [row] (empty? (clojure.set/difference #{1 2 3 4 5 6 7 8 9}
                                                         row)))
               (rows board))))

(defn cols [board]
  (vec
   (map (fn [c] (col-values board [0 c])) (range 0 9))))

(defn valid-cols? [board]
  (every? true?
          (map (fn [col] (empty? (clojure.set/difference #{1 2 3 4 5 6 7 8 9}
                                                         col)))
               (cols board))))

(defn blocks [board]
  (vec
   (map (fn [coord] (block-values board coord))
        (for [x [0 3 6]
              y [0 3 6]]
          [x y]))))

(defn valid-blocks? [board]
  (every? true?
          (map (fn [block] (empty? (clojure.set/difference #{1 2 3 4 5 6 7 8 9}
                                                           block)))
               (blocks board))))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  nil)

(defn find-empty-point [board]
  nil)

(defn solve [board]
  nil)
