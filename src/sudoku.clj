(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn ?-to-0 [string]
  (replace string #"\\?" "0"))


(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))


(defn has-value? [board coord]
  (not= 0 (value-at board coord)))


(defn row-values [board coord]
  (into #{} (board (first coord))))


(defn col-values [board coord]
  (into #{} (map #(get %1 (second coord)) board)))



(defn coord-pairs [coords]
  (into [] (for [x coords
                 y coords]
             [x y])))


(defn block-values [board coord]
  (let [x (* 3 (quot  (first coord) 3))]
    (let [y (* 3 (quot (second coord) 3))]
     (into #{}
           (map #(value-at board %1)
                (apply concat (map #(into #{} [[x %1][(+ x 1) %1] [(+ x 2) %1]])
                                   (range y (+ y 3)))))))))


(defn valid-values-for [board coord]
  (if (not= 0 (value-at board coord))
    #{}
    (set/difference all-values
                    (block-values board coord)
                    (row-values board coord)
                    (col-values board coord))))

(defn filled? [board]
  (reduce #(and
            (not (contains? (into #{} %2) 0))
            %1)
          true
          board))


(defn rows [board]
  (map #(into #{} %1) board))


(defn valid-rows? [board]
  (reduce #(and %1 (= (into #{} (range 1 10)) %2)) true (rows board)))




(defn cols [board]
  (if (empty? (first board))
    ()
  (cons
   (reduce #(conj %1 (first %2)) #{} board)
   (cols (reduce #(cons (rest %2) %1) []  board)))))


(defn valid-cols? [board]
  (reduce #(and %1 (= (into #{} (range 1 10)) %2)) true (cols board)))

(defn blocks [board]
  (for [x [0 3 6]
         y [0 3 6]]
          (block-values board [x y])))




(defn valid-blocks? [board]
  (reduce #(and %1 (= (into #{} (range 1 10)) %2)) true (blocks board)))

(defn valid-solution? [board]
  (and (valid-cols? board) (valid-rows? board) (valid-blocks? board)))



(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter identity (for [x (range 9)
        y (range 9)]
    (if (= 0 (value-at board [x y]))
      [x y])))))


(defn solve [board]
  (if (filled? board)
    (if (valid-solution? board)
      board
      nil)
    (let [empty-point (find-empty-point board)]
      (for [value (valid-values-for board empty-point)
            solution (solve (set-value-at board empty-point value))]
        solution))))


