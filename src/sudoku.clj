(ns sudoku
  (:require [clojure.set :as set]))
(def all-values #{1 2 3 4 5 6 7 8 9})
(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (= 0 (value-at board coord))
    false
    true))

(defn row-values [board coord]
  (let [[row col] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[row col] coord
        getter (fn [vec]
                 (get vec col))]
    (set (map getter board))))

(defn coord-pairs [coords]
  (for [row coords
        col coords]
    [row col]))

(defn top-left [board coord]
  (let [[row col] coord]
    [(* 3(quot row 3)) (* 3 (quot col 3))]))

(defn block-values [board coord]
  (let [[row col] (top-left board coord)
        adds [0 1 2]]
    (set (for [num adds
               colss adds]
           (value-at board [(+ num row) (+ colss col)])))))

(defn valid-values-for [board coord]
  (if (empty? coord)
    #{}
    (let [r-vals (row-values board coord)
          c-vals (col-values board coord)
          b-vals (block-values board coord)]
      (if (has-value? board coord)
        #{}
        (set/difference
         all-values
         (set/union r-vals c-vals b-vals))))))

(defn filled? [board]
  (not (contains? (set (apply set/union board)) 0)))

(defn rows [board]
  (for [row board]
    (set row)))

(defn valid-rows? [board]
  (let [row-vals (rows board)]
    (not (contains? (set (for [row row-vals]
                           (= all-values row))) false))))

(defn cols [board]
 (let [cols (map dec all-values)]
    (for [num cols]
      (col-values board [0 num]))))

(defn valid-cols? [board]
  (let [col-vals (cols board)]
    (not (contains? (set (for [col col-vals]
                           (= all-values col))) false))))

(defn blocks [board]
  (let [coords [0 3 6]]
    (for [row coords
          col coords]
      (block-values board [row col]))))

(defn valid-blocks? [board]
  (let [block-vals (blocks board)]
    (not (contains? (set (for [block block-vals]
                           (= all-values block))) false))))

(defn valid-solution? [board]
  (let [rows? (valid-rows? board)
        cols? (valid-cols? board)
        blocks? (valid-blocks? board)]
    (and rows? cols? blocks?)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (let [vals (map dec all-values)]
      (if (filled? board)
        []
        (first (filter (fn [val]
                         (not (nil? val)))
                       (for [row vals
                             col vals]
                         (when (= 0(value-at board [row col])) [row col])))))))


(defn solve-helper [board]
  )

(defn solve [board]
(let [coord (find-empty-point board)
        valid-vals (valid-values-for board coord)
        ]
    (cond
     (filled? board) (if (valid-solution? board)
                       board
                       #{}
                       )
     :else (for [elem valid-vals
                 solution (solve (set-value-at board coord elem))]
             solution))))
