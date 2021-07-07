(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def board-size 3)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not= 0 (value-at board coord)))

(defn row-values [board coord]
  (let [row (first coord)
        values (get board row)]
    (into #{} values)))

(defn col-values [board coord]
  (let [col-value (fn [c] (fn [v] (get v c)))
        c (second coord)]
    (into #{} (map (col-value c) board))))

(defn coord-pairs [coords]
  (for [r coords 
        c coords]
    [r c]))

(defn top-left [coord]
  (let [r (first coord)
        c (second coord)
        scale (fn [x] (* board-size (int (/ x board-size))))]
    [(scale r) (scale c)]))

(defn block-values [board coord]
  (let [tl (top-left coord)
        r (first tl)
        c (second tl)]
   (set  
    (for [row (range r (+ r board-size))
          col (range c (+ c board-size))]
      (value-at board  [row col])))))

(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
      (let [values-present 
        (set/union (row-values board coord)
                   (col-values board coord)
                   (block-values board coord))]
    (set/difference all-values values-present))))

(defn values-on-board [board]
  (reduce #(into %1 %2) #{} board))

(defn filled? [board]
  (let [not-contains (complement contains?)]
    (not-contains (values-on-board board) 0)))

(defn rows [board]
  (let [to-set (fn [v r] (conj v (set r)))]
    (reduce to-set [] board)))

(defn valid-coords? [coords]
  (every? #(= % all-values) coords))

(defn valid-rows? [board]
  (valid-coords? (rows board)))

(defn cols [board]
  (let [size (* board-size board-size)
        coords 
        (map vector (repeat (inc size) 0) (range (* board-size board-size)))
        col-values-helper 
        (fn [board] (fn [coord] (col-values board coord)))]
    (into [] (map (col-values-helper board) coords))))

(defn valid-cols? [board]
  (valid-coords? (cols board)))

(defn top-left-corners []
  (let [whole-numbers (iterate inc 0)
        multiples (map #(* board-size %) 
                       (take board-size whole-numbers))]
    (coord-pairs multiples)))

(defn blocks [board]
  (let [coords (fn [board]
                 (fn [coord] (block-values board coord)))]
    (into [] (map (coords board) (top-left-corners)))))

(defn valid-blocks? [board]
  (valid-coords? (blocks board)))

(defn valid-solution? [board]
  (and
   (valid-rows? board)
   (valid-cols? board)
   (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn indexed-board [board]
  (let [inner-indexed (map #(map-indexed vector %) board)
        indexed (map-indexed vector inner-indexed)]
    indexed))

(defn find-first-index [a-seq pred]
  (if
   (empty? a-seq) nil
   (let [head (first a-seq)
         tail (rest a-seq)
         [idx itm] head]
     (if (pred itm) idx
         (find-first-index tail pred)))))

(defn find-empty-point [board]
  (let [indexed (indexed-board board)]
    (loop [rows indexed]
      (if (empty? rows) nil
          (let [[outer-idx itm] (first rows)
                inner-idx (find-first-index itm zero?)]
            (if (nil? inner-idx) 
              (recur (rest rows)) 
              [outer-idx inner-idx]))))))


(defn solve [board]
   (let [coord (find-empty-point board)]
     (cond 
      (and (nil? coord) (valid-solution? board)) board
      (nil? coord) '()
      :else (for [valid-value (valid-values-for board coord)
                 result (solve (set-value-at board coord valid-value))]
             result))))
