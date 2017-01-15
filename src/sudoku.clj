(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (zero? (value-at board coord))))

(defn row-values [board coord]
  (apply conj #{} (get board (get coord 0))))

(defn col-values [board coord]
  (loop [r 0
         s #{}]
  (if (>= r (count board))
    s
  (recur (inc r) (conj s (get (get board r) (get coord 1)))))))

(defn coord-pairs [coords]
  (seq
    (for [row coords col coords]
      [row col])))

(defn left-up[coords]
  (let [f (fn[z] (* (int (/ (get coords z) 3)) 3))]
  (vector (f 0) (f 1))))

(defn block-values [board coord]
  (apply conj #{} (map (fn[f] (get-in board f))
                  (map (fn[f] (map + (left-up coord) f)) (coord-pairs [0 1 2])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
   #{}
  (set/difference all-values
    (set/union (row-values board coord)
    (set/union (col-values board coord) (block-values board coord))))))

(defn board-values [board]
  (loop [r 0
         s #{}]
  (if (>= r (count board))
   s
  (recur (inc r) (apply conj s (get board r))))))

(defn filled? [board]
  (not (contains? (board-values board) 0)))

(defn rows [board]
  (map set board))

(defn valid-rows? [board]
  (= (rows board) (repeat (count all-values) all-values)))

(defn cols [board]
  (loop [r 0
         s []]
  (if (>= r (count board))
   s
   (recur (inc r) (conj s (col-values board [0 r]))))))

(defn valid-cols? [board]
  (= (cols board) (repeat (count all-values) all-values)))

(defn blocks [board]
  (for [x [0 3 6] y [0 3 6]]
  (block-values board [x y])))

(defn valid-blocks? [board]
  (= (blocks board) (repeat (count all-values) all-values)))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [loc (mapcat (partial remove nil?) (for [x (range 9) y (range 9)]
    (if (zero? (value-at board [x y])) [x y])))]
    [(first loc) (second loc)]))

(defn solve [board]
  (first (letfn [(solver [board]
          (let [empty-point (find-empty-point board)
                values (valid-values-for board empty-point)]
          (mapcat (fn [val]
                    (let [board (set-value-at board empty-point val)]
                    (cond
                      (valid-solution? board) [board]
                      (filled? board) nil
                      :else (lazy-seq (solver board))))) values)))]
    (solver board))))
