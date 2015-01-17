(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board [(first coord) (second coord)]))

(defn has-value? [board coord]
  (< 0 (value-at board coord)))

(defn row-values [board coord]
  (set (get board (first coord))))

(defn col-values [board coord]
  (let [col (second coord)
        pick (fn pick [depth]
               (if (< depth 9)
               (cons (value-at board [depth col]) (pick (inc depth)))
               []))]
    (set (pick 0))))

(defn coord-pairs [coords]
  (sort (vec (set (for [a coords
                        b coords]
                    [a b])))))

(defn block-values [board coord]
  (let [top (fn [x]                     ; top for "top leftcorner"
              (* 3 (int (/ x 3))))
        xtl (top (first coord))         ; xtl "for x of top left"
        ytl (top (second coord))
        tl [xtl ytl]
        shifts (coord-pairs [0 1 2])
        coords (for [s shifts] (map + tl s))]
    (set (map value-at (repeat board) coords))))   ; repeat isn't probably the smartest way to do this, but at least keeps the code simle.

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (let [row (row-values board coord)
        col (col-values board coord)
        block (block-values board coord)
        invalid (set/union row block col)]
    (set/difference all-values invalid))))

(defn filled? [board]
  (let [rows-as-sets (map set board)
        values (apply set/union rows-as-sets)]
    (not (= 0 (some #{0} values)))))

(defn rows [board]
  (for [r (range 0 9)]
    (row-values board [r 0])))

(defn valid-rows? [board]
  (let [counts (map count (rows board))]
    (every? (fn [x] (== x 9)) counts)))

(defn cols [board]
  (for [c (range 0 9)]
    (col-values board [0 c])))

(defn valid-cols? [board]
  (let [counts (map count (cols board))]
    (every? (fn [x] (== x 9)) counts)))

(defn blocks [board]
  (for [x [0 3 6]
        y [0 3 6]]
    (block-values board [x y])))

(defn valid-blocks? [board]
  (let [counts (map count (blocks board))]
    (every? (fn [x] (== x 9)) counts)))

(defn valid-solution? [board]
  (and (filled? board)
       (valid-cols? board)
       (valid-rows? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]     ; take-until, or something like that might have been better, but the array being two dimensional makes it harder to use.
  (let [finder (fn finder [x y]
                 (cond
                   (and (== x 9) (== y 9))
                     nil
                   (== 0 (value-at board [x y]))
                     [x y]
                   (== y 8)
                     (finder (inc x) 0)
                   :else
                     (finder x (inc y))))]
    (finder 0 0)))


(defn solve [board]
  (cond
   (valid-solution? board)
    board
   (filled? board)
    nil
   :else
    (let [next-empty (find-empty-point board)
          possibilities (valid-values-for board next-empty)
          forks (for [p possibilities]
                  (solve (set-value-at board next-empty p)))]
      (last (sort-by count forks)))))
