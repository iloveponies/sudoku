(ns sudoku
  (:require [clojure.set :as set]))

(def all-values #{1 2 3 4 5 6 7 8 9})

(def board identity)

(defn value-at [board coord]
  (get-in board coord))


(defn has-value? [board coord]
  ((complement =) (value-at board coord) 0))


(defn row-values [board [row col]]
  (set (get board row)))


(defn col-values [board [row col]]
  (set (map (fn [x] (get x col)) board)))


(defn coord-pairs [coords]
  (for [r coords
        c coords]
    [r c]))


(defn block-values [board coord]
  (let [corner-coords (map (fn [x] (* (int (/ x 3)) 3)) coord)
        box-coords (for [cs (coord-pairs [0 1 2])]
                     (map + corner-coords cs))]
    (set (map (fn [c] (value-at board c)) box-coords))))


(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (set/difference all-values
      (set/union (row-values board coord)
                 (col-values board coord)
                 (block-values board coord)))))


(defn filled? [board]
  (loop [acc true
         coords (coord-pairs (range 9))]
    (if (or (empty? coords) (not acc))
        acc
        (recur (and acc (has-value? board (first coords)))
           (rest coords)))))


(defn rows [board]
  (map set board))


(defn valid-rows? [board]
  (every? (fn [x] (= (count x) 9)) (rows board)))


(defn cols [board]
  (if (some empty? board)
    #{}
    (cons (set (map first board)) (cols (map rest board)))))


(defn valid-cols? [board]
  (every? (fn [x] (= (count x) 9)) (cols board)))


(defn blocks [board]
  (let [corners (coord-pairs [0 3 6])]
    (map (fn [x] (block-values board x)) corners)))


(defn valid-blocks? [board]
  (every? (fn [x] (= (count x) 9)) (blocks board)))


(defn valid-solution? [board]
  (and (filled? board)
       (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))


(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))


(defn find-empty-point [board]
  (loop [cs (coord-pairs (range 9))]
    (cond
      (empty? cs) nil
      ((complement has-value?) board (first cs)) (first cs)
      :else (recur (rest cs)))))



;; (defn solve-helper [board coords poss-vals]
;;   (if (filled? board) ; or no more poss values?
;;     (if (valid-solution? board)
;;       board
;;       '()) ; filled it wrong
;;     (for [ elem poss-vals  ;for each element in remaining possiblities
;;       :let [ newboard (set-value-at board coords elem)
;;              newcoords (find-empty-point newboard)
;;              newposs (valid-values-for newboard newcoords)
;;              solution (solve-helper newboard newcoords newposs)]]
;;         solution)))
;;
;; (defn solve [board]
;;   (let [coords (find-empty-point board)]
;;     (solve-helper board coords (valid-values-for board coords))))
;;
;; => produces a million parantheses with the solution embedded somwhere in it...




(defn solve [board]
  (if (valid-solution? board)
    board
    (let [coords (find-empty-point board)]
      (for [elem (valid-values-for board coords)
            solution (solve (set-value-at board coords elem))]
        solution))))
