(ns sudoku
  (:require [clojure.set :as set]))

(def all-values #{1 2 3 4 5 6 7 8 9})


(def board identity)


(defn value-at [board coord]
  (get-in board coord))


(defn has-value? [board coord]
  (not(== (value-at board coord) 0)))

(defn row-values [board coord]
  (set(get board (get coord 0))))


(defn col-values [board coord]
        (loop [row 0
               col (get coord 1)
               col-vals #{}]
          (if (< row 9)
            (recur
             (inc row)
              col
             (conj col-vals (value-at board [row col])))

            col-vals)))

(defn coord-pairs [coords]
   (for [row coords
        col coords]
    (vector row col)))


(defn block-values [board coord]
  (let [top-left (fn []
                   [(- (coord 0) (mod (coord 0) 3))
                    (- (coord 1) (mod (coord 1) 3))])
        add-top-left (fn [a-coord]
                      (map + (top-left) a-coord))
        block-coords (fn []
                       (map add-top-left (coord-pairs [0 1 2])))
        coord-vals (fn [a-coord]
                 (value-at board a-coord))]

    (set(map coord-vals (block-coords)))))



(defn valid-values-for [board coord]
  (if (has-value? board coord)
   #{}
  (set/difference all-values (col-values board coord) (row-values board coord) (block-values board coord))))


(defn filled? [board]
  (let [all-numbers (fn []
                      (loop [row 0
                             col 0
                             seq []]
                        (cond
                         (== 9 row) seq
                         (== 9 col) (recur (inc row) 0 seq)
                         :else (recur row (inc col) (conj seq (value-at board [row col]))))))]
    (not (contains? (set(all-numbers)) 0 ))))


(def rows-and-cols [[0 0]
                     [1 1]
                     [2 2]
                     [3 3]
                     [4 4]
                     [5 5]
                     [6 6]
                     [7 7]
                     [8 8]])
(def block-coords [[0 0] [0 3] [0 6]
                   [3 0] [3 3] [3 6]
                   [6 0] [6 3] [6 6]])


(defn rows-cols-blocks-helper [board f sample-coords]
  (let [ get-row-or-col (fn [coord]
           (f board coord))]

     (vec(map get-row-or-col sample-coords))))


(defn valid-seq? [a-seq]
  (let [valid-subset? (fn [subset]
                       (loop [n 1]
                         (cond
                         (== n 10) true
                         (contains? subset n) (recur (inc n))
                         :else false)))]
    (loop [n 0]
     (cond
     (== n 9) true
     (valid-subset? (get a-seq n)) (recur (inc n))
     :else false))))

(defn rows [board]
  (rows-cols-blocks-helper board row-values rows-and-cols))



(defn valid-rows? [board]
  (valid-seq? (rows board)))

(defn cols [board]
   (rows-cols-blocks-helper board col-values rows-and-cols))


(defn valid-cols? [board]
  (valid-seq? (cols board)))


(defn blocks [board]
  (rows-cols-blocks-helper board block-values block-coords))


(defn valid-blocks? [board]
  (valid-seq? (blocks board)))


(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))


(defn find-empty-point [board]
  (loop [row 0
         col 0]
    (cond
     (== row 9) nil
     (== col 9) (recur (inc row) 0)
     (not (has-value? board [row col])) [row col]
     :else (recur row (inc col)))))


(defn solve-helper [board]
    (if (filled? board)
    (if (valid-solution? board)
      (first board)
      [])

    (let [coord (find-empty-point board)
          do-solve (fn [a-value]
                     (solve-helper (set-value-at board coord a-value)))]
            (map do-solve (valid-values-for board coord)))))


(defn solve [board]
    (if (filled? board)
    (if (valid-solution? board)
      board
      [])

    (let [coord (find-empty-point board)]
        (first(filter (complement empty?) (for [a-value (valid-values-for board coord)]
          (solve (set-value-at board coord a-value))))))))






