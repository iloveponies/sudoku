(ns sudoku.core
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (= 0 (value-at board coord))))

(defn row-values [board [row _]]
  (set (get board row)))

(defn col-values [board [_ column]]
  (let [col (fn [a b] (conj a (get b column)))]
    (reduce col #{} board)))

(defn coord-pairs [coords]
  (for [a coords
        b coords]
    [a b]))

(defn block-values [board coord]
  (let [box (coord-pairs [0 1 2])
        shift-coord (fn [bo [dx dy]] 
                      (map (fn [[x y]]
                             [(+ x dx)
                              (+ y dy)])
                           bo))
        get-topleft (fn [[x y]]
                      [(* 3 (int (/ x 3)))
                       (* 3 (int (/ y 3)))])]
    (set (map (fn [c] (value-at board c))
              (shift-coord box (get-topleft coord))))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values 
                    (set/union (block-values board coord)
                               (row-values board coord)
                               (col-values board coord)))))

(defn filled? [board]
  (not (contains? (reduce
                   (fn [a b]
                    (set/union a (set b)))
                   #{}
                   board)
                  0)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (every? (fn [a] (= all-values a)) (rows board)))

(defn cols [board]
 (let [stop (count board)]
  (loop [col 0
         colset []]
    (if (= col stop)
      colset
      (recur (inc col) (conj colset (col-values board [0 col])))))))

(defn valid-cols? [board]
  (every? (fn [a] (= all-values a)) (cols board)))

(defn blocks [board]
  (map (fn [a] (block-values board a)) (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (every? (fn [a] (= all-values a)) (blocks board)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first 
   (drop-while 
    (fn [a] (has-value? board a))
    (coord-pairs [0 1 2 3 4 5 6 7 8]))))

(defn solve-helper [board]
 (if (filled? board)
    (if (valid-solution? board)
     [board]
     [])
    (let [next-empty (find-empty-point board)]
      (for [valid (valid-values-for board next-empty)
            solution (solve-helper (set-value-at board next-empty valid))
            :when (not (empty? solution))]
        solution))))

(defn solve [board]
  (first(solve-helper board)))