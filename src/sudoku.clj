(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (let [[x y] coord
        x-row (get board x)]
    (get x-row y)))

(defn has-value? [board coord]
  (not= (value-at board coord) 0))

(defn row-values [board coord]
  (let [[x] coord
        x-row (get board x)]
    (set x-row)))

(defn col-values [board coord]
  (let [[_ y] coord
        values (fn [x a-set] (if (= x 9)
                              a-set
                              (recur (inc x)
                                     (conj a-set (get (get board x) y)))))]
    (values 0 #{})))

(defn coord-pairs [coords]
  (let [size (count coords)
        counter-limit (* size size)
        helper (fn [a-vector counter] (if (>= counter counter-limit)
                                        a-vector
                                        (recur (conj a-vector (vector (get coords (int (/ counter size)))
                                                                      (get coords (mod counter size))))
                                               (inc counter))))]
    (helper [] 0)))

(defn block-values [board coord]
  (let [[x1 y1] coord
        top-left-coord (vector (* (int (/ x1 3)) 3)
                               (* (int (/ y1 3)) 3))
        [top-x left-y] top-left-coord
        values (fn [row a-set] (if (>= (- row top-x) 3)
                                       a-set
                                       (recur (inc row)
                                              (conj a-set (value-at board [row left-y])
                                                          (value-at board [row (+ left-y 1)])
                                                          (value-at board [row (+ left-y 2)])))))]
    (values top-x #{})))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
      #{}
      (let [values #{1 2 3 4 5 6 7 8 9}
            invalid-values (clojure.set/union (row-values board coord) (col-values board coord) (block-values board coord))]
        (clojure.set/difference values invalid-values))))

(defn filled? [board]
  (let [all-values (set (concat (get board 0)
                                (get board 1)
                                (get board 2)
                                (get board 3)
                                (get board 4)
                                (get board 5)
                                (get board 6)
                                (get board 7)
                                (get board 8)))]
    (not (contains? all-values 0))))

(defn groups [group-values coordinate-for-index board]
  (let [helper (fn [index a-vector] (if (> index 8)
                                   a-vector
                                   (recur (inc index) (conj a-vector (group-values board (coordinate-for-index index))))))]
    (helper 0 [])))

(defn rows [board]
  (let [coordinate-for-row (fn [row]
                              (vector row 0))]
    (groups row-values coordinate-for-row board)))


(defn cols [board]
  (let [coordinate-for-col (fn [col]
                              (vector 0 col))]
    (groups col-values coordinate-for-col board)))


(defn blocks [board]
  (let [coordinate-for-block (fn [block]
                                (vector (* 3 (int (/ block 3)))
                                        (* 3 (int (mod block 3)))))]
    (groups block-values coordinate-for-block board)))

(defn valid [group board]
    (if (not (filled? board))
      false
      (let [group-set (group board)
                 helper (fn [index] (cond (> index 8) true
                                        (not= (count (get group-set index)) 9) false
                                        :else (recur (inc index))))]
             (helper 0))))

(defn valid-rows? [board]
  (valid rows board))

(defn valid-cols? [board]
  (valid cols board))

(defn valid-blocks? [board]
   (valid blocks board))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (let [[x y] coord
        row-in-question (get board x)
        new-row (assoc row-in-question y new-value)]
    (assoc board x new-row)))

(defn find-empty-point [board]
  (let [find-zero-from-row (fn [row index]
                               (cond (> index 8) -1
                                     (zero? (get (get board row) index)) index
                                     :else (recur row (inc index))))
        find-zero-from-board (fn [x]
                                 (let [y (find-zero-from-row x 0)]
                                   (if (= y -1)
                                       (recur (inc x))
                                       (vector x y))))]
    (find-zero-from-board 0)))

(defn solve-help [board]
  (if (filled? board)
      [board]
      (let [coord (find-empty-point board)
                       valid-values (valid-values-for board coord)]
                   (for [value valid-values
                         solution (solve-help (set-value-at board coord value))]
                         solution))))

(defn solve [board]
  (first (solve-help board)))

