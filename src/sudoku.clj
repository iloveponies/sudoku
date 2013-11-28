(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))


(defn has-value? [board coord]
  (not (== (value-at board coord) 0)))


(defn row-values [board coord]
  (let [[row col] coord]
    (set (get board row))
    ))

(defn col-values [board coord]
  (let [[row col] coord]
     (loop [pos 0
            set #{}
            ]
       (cond
        (== pos 9) set
        :else (recur (inc pos) (conj set (value-at board [pos col])))))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn block-values [board coord]
  (let [[row col] coord
        top-coord (fn [val]
                (- val (mod val 3)))
        my-range (fn [val]
                   (range (top-coord val) (+ (top-coord val) 3)))
        block (fn [val1 val2]
                (for [row (my-range val1)
                      col (my-range val2)]
                  [row col]))
        reduce-helper (fn [a-seq token]
                         (set (cons (value-at board token) a-seq))
                         )]
    (reduce reduce-helper [] (block row col))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference (set/difference (set/difference all-values (block-values board coord))
                                    (col-values board coord))
                    (row-values board coord))))

(defn filled? [board]
  (let [helper (fn [val coord]
                 (if (not (has-value? board coord))
                    false
                    val))]
    (reduce helper true (coord-pairs (range 9)))))

(defn rows [board]
  (let [helper (fn [ret row-num]
                 (conj ret (row-values board [row-num 0])))]
    (reduce helper [] (range 9))))



(defn cols [board]
  (let [helper (fn [ret col-num]
                 (conj ret (col-values board [0 col-num])))]
    (reduce helper [] (range 9))))


(defn blocks [board]
  (let [helper (fn [ret coord]
                 (conj ret (block-values board coord)))]
    (reduce helper [] (coord-pairs [0 3 6]))))

(defn valid-rows? [board]
  (let [helper (fn [val row]
                 (if (= all-values row)
                   val
                   false))]
    (reduce helper true (rows board))))

(defn valid-cols? [board]
 (let [helper (fn [val col]
                 (if (= all-values col)
                   val
                   false))]
    (reduce helper true (cols board))))


(defn valid-blocks? [board]
  (let [helper (fn [val block]
                 (if (= all-values block)
                   val
                   false))]
    (reduce helper true (blocks board))))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [helper (fn [empty-coord point]
                 (if (has-value? board point)
                   empty-coord
                   point))]
    (reduce helper [] (coord-pairs (range 9)))))

(defn solve [current-board]
  (cond
   (valid-solution? current-board) current-board
   (and (filled? current-board) (not (valid-solution? current-board))) ()
   (empty? (valid-values-for current-board (find-empty-point current-board))) ()
   :else (let [empty-point (find-empty-point current-board)]
            (for [value (valid-values-for current-board empty-point)
                  new-board (solve (set-value-at current-board empty-point value))]
              new-board))))
