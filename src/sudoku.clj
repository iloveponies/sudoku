(ns sudoku 
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (= (value-at board coord) 0)
    false
    true
  ))

(defn row-values [board coord]
  (let  [[y -] coord]
    (set (get board y))
  ))

(defn col-values [board coord]
  (let [[- x] coord]
    (set (for [rivi board] (get rivi x))
    )))


;(defn coord-pairs [coords]
  ;(loop [naatit coords tulos []]
    ;"Tämä on aivan toimiva, mutta tarpeettoman monimutkainen ensimmäinen versio."
    ;(if (empty? naatit)
      ;tulos
      ;(recur 
        ;(rest naatit)
        ;(apply vector (concat ;lisätään parit, joissa x<y  (1)
            ;(apply vector (concat ;parit, joissa x>=y   (1)
                ;tulos 
                ;(apply vector (apply concat (for [luku naatit] (vector [(first naatit) luku]))))))  ;lisätään parit, joissa x>=y  (2)
            ;(apply vector (apply concat (for [luku2 (rest naatit)] (vector [luku2 (first naatit)]))))))  ;lisätään parit, joissa x<y  (2)
      ;))))

(defn coord-pairs [coords]
  (apply concat (for [koord1 coords]
      (for [koord2 coords]
        [koord1 koord2])
    )))


(defn kulmakoordinaatit [coord]
  (let [[x y] coord]
    (cond
      (< x 3) (cond
        (< y 3) [0 0]
        (< y 6) [0 3]
        :else [0 6]
      )
      (< x 6) (cond
        (< y 3) [3 0]
        (< y 6) [3 3]
        :else [3 6]
      )
      :else (cond
        (< y 3) [6 0]
        (< y 6) [6 3]
        :else [6 6]
      )
    ))
)

(defn block-values [board coord]
  (set (for [[x2 y2] (coord-pairs [0 1 2])]
      (let [[x1 y1] (kulmakoordinaatit coord)] (value-at board [(+ x2 x1) (+ y2 y1)])))
  ))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    #{}
    (clojure.set/difference #{0 1 2 3 4 5 6 7 8 9} (clojure.set/union (row-values board coord) (col-values board coord) (block-values board coord))
    ))
)

(defn kaikki-arvot [board]
  (apply clojure.set/union (for [rivi board] (set rivi))
  )
)

(defn filled? [board]
  (not (contains? (kaikki-arvot board) 0)))

(defn rows [board]
  (apply vector (for [rivi board]
      (set (for [arvo rivi] arvo))
    )))

(defn valid-rows? [board]
  (and 
    (empty? (filter (fn [x] (contains? x 0)) (rows board)))
    (empty? (filter (fn [y] (not (= (count y) 9))) (rows board))))
)

(defn cols [board]
  (apply vector (for [luku [0 1 2 3 4 5 6 7 8]]
      (set (for [rivi board]
          (get rivi luku))
      )))
)

(defn valid-cols? [board]
  (and 
    (empty? (filter (fn [x] (contains? x 0)) (cols board)))
    (empty? (filter (fn [y] (not (= (count y) 9))) (cols board)))
  ))

(defn blocks [board]
  (apply vector 
    (for [koord (coord-pairs [0 3 6])]
      (set (block-values board koord))
    ))
)

(defn valid-blocks? [board]
  (and 
    (empty? (filter (fn [x] (contains? x 0)) (blocks board)))
    (empty? (filter (fn [y] (not (= (count y) 9))) (blocks board)))
  ))

(defn valid-solution? [board]
  (cond
    (not (filled? board)) false
    (not (valid-rows? board)) false
    (not (valid-rows? board)) false
    (not (valid-blocks? board)) false
    :else true
  ))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value
  ))

(defn find-empty-point [board]
  (first (filter 
      (fn [koord] (= (value-at board koord) 0))
      (coord-pairs [0 1 2 3 4 5 6 7 8])
    ))
)

(defn solve [board]
  (if (valid-solution? board)
    board
    (if (filled? board)
      []
      (first (filter valid-solution? 
          (for [arvaus (valid-values-for board (find-empty-point board))]
            (solve (set-value-at board (find-empty-point board) arvaus)))))
    )))
