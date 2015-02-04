(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord] (get-in board coord))

(defn has-value? [board coord] (not (= 0 (value-at board coord))))

(defn row-values [board coord] (reduce (fn [row elem] (conj row elem)) #{} (get board (get coord 0))))

(defn col-values [board coord] (reduce (fn [col elem-row] (conj col (get elem-row (get coord 1)))) #{} board))

(defn coord-pairs [coords] (reduce (fn [pairs elem] (concat pairs (for [one coords] [elem one]))) [] coords))

(defn block-values [board coord] (let [x (get coord 0)
                                       y (get coord 1)
                                       top-left-coord [(- x (mod x 3)) (- y (mod y 3))]
                                       block-coords (for[x-val (range (get top-left-coord 0) (+ (get top-left-coord 0) 3))
                                                         y-val (range (get top-left-coord 1) (+ (get top-left-coord 1) 3))]
                                                          [x-val y-val])]
                                         (reduce (fn [values elem] (conj values (value-at board elem))) #{} block-coords)))

(defn valid-values-for [board coord] (if (has-value? board coord)
                                       #{}
                                       (reduce
                                        (fn [values elem]
                                          (if (or (contains? (block-values board coord) elem) (contains? (row-values board coord) elem) (contains? (col-values board coord) elem))
                                            values
                                            (conj values elem)))
                                        #{}
                                        [1 2 3 4 5 6 7 8 9])))

(defn filled? [board] (let [to-set (fn [a-seq] (reduce (fn [a-set elem] (conj a-set elem)) #{} a-seq))]
                        (not (contains? (to-set (reduce (fn [values elem] (concat values elem)) #{} board)) 0))))

(defn rows [board] (reduce (fn [all-rows elem] (conj all-rows (row-values board [elem 0]))) [] (range 0 (count board))))

(defn valid-rows? [board] (let [is-valid? (fn [a-set] (if (= (count a-set) (count board))
                                                        (reduce (fn [valid elem] (if (contains? a-set elem) valid false)) true (range 1 (+ (count board) 1)))
                                                        false))]
                            (reduce (fn [valid row] (if (is-valid? row) valid false)) true (rows board))))

(defn cols [board] (reduce (fn [all-cols elem] (conj all-cols (col-values board [0 elem]))) [] (range 0 (count board))))

(defn valid-cols? [board] (let [is-valid? (fn [a-set] (if (= (count a-set) (count board))
                                                        (reduce (fn [valid elem] (if (contains? a-set elem) valid false)) true (range 1 (+ (count board) 1)))
                                                        false))]
                            (reduce (fn [valid col] (if (is-valid? col) valid false)) true (cols board))))

(defn blocks [board] (let [all-block-coords (coord-pairs (reduce (fn [div-by-three elem] (if (= 0 (mod elem 3)) (conj div-by-three elem) div-by-three)) [] (range 0 (count board))))]
                       (reduce (fn [all-blocks elem] (conj all-blocks (block-values board elem))) [] all-block-coords)))

(defn valid-blocks? [board] (let [is-valid? (fn [a-set] (if (= (count a-set) (count board))
                                                        (reduce (fn [valid elem] (if (contains? a-set elem) valid false)) true (range 1 (+ (count board) 1)))
                                                        false))]
                            (reduce (fn [valid block] (if (is-valid? block) valid false)) true (blocks board))))

(defn valid-solution? [board] (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value] (assoc-in board coord new-value))

(defn find-empty-point [board] (first (reduce (fn [empty-coords elem] (if (has-value? board elem) empty-coords (conj empty-coords elem))) '() (coord-pairs (range 0 (count board))))))


(defn solve [board] (if (= nil (find-empty-point board))
                      (if (valid-solution? board)
                        board
                        '())
                      (let [empty-point (find-empty-point board)
                            valid-values (valid-values-for board empty-point)]
                       (for [valid-value valid-values
                              solution (solve (set-value-at board empty-point valid-value))]
                         solution))))
