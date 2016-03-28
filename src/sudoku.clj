(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(def value-at get-in)

(defn has-value? [board coord]
  (not (= (value-at board coord) 0)))

(defn row-values [board [row _]]
  (set (get board row)))

(defn col-values [board [_ col]]
  (set (map #(get % col) board)))

(defn coord-pairs [coords]
  (apply vector
         (for [x coords
               y coords]
           [x y])))

(defn block-values [board coord]
  (let [first-ind (fn [i] (- i (rem i 3)))
        [r-min c-min] (map first-ind coord)
        [r-end c-end] (map #(+ % 3) [r-min c-min])]
    (set (for [r (range r-min r-end)
          c (range c-min c-end)]
      (value-at board [r c])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (reduce #(set/difference %1 (%2 board coord)) all-values [row-values col-values block-values])))


(defn filled? [board]
  (let [coords (coord-pairs (range 0 10))
        has-vals (map #(has-value? board %1) coords)]
    (every? identity has-vals)))

(defn rows [board]
  (apply vector (map set board)))

(defn valid-x [f board]
  (every? #(= % all-values) (f board)))

(defn valid-rows? [board]
  (valid-x rows board))

(defn cols [board]
  (map set (apply mapv vector board)))

(defn valid-cols? [board]
  (valid-x cols board))

(defn blocks [board]
  (map set
       (for [x (range 0 9 3) y (range 0 9 3)]
         (block-values board [x y]))))

(defn valid-blocks? [board]
  (valid-x blocks board))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(def set-value-at assoc-in)

(defn find-empty-point [board]
  (loop [r 0 c 0]
    (cond
      (not (has-value? board [r c]))
      [r c]
      (>= r 9)
      nil
      :else
      (let [new-c (mod (inc c) 9)
            new-r (if (= 0 new-c) (inc r) r)]
        (recur new-r new-c)))))

(defn solve [board]
  (let
      [empty-coords (find-empty-point board)]
      (if (not empty-coords)
        (if (valid-solution? board) board nil)
        (let [test-vals (reduce set/difference all-values (map #(% board empty-coords) [row-values col-values block-values]))]
          (if (empty? test-vals)
            nil
            (first (filter identity
                    (for [test-val test-vals]
                      (solve (set-value-at board empty-coords test-val))))))))))
