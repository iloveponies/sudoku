(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (not (== 0 (value-at board coord))))

(defn row-values [board coord]
  (let [[x y] coord]
    (loop [st #{}
           yl 0]
      (if (>= yl 9)
        st
        (recur (conj st (value-at board [x yl])) (inc yl))))))
    ;(reduce (fn [st y] (conj st (value-at board [x y]))  #{} )))

(defn col-values [board coord]
  (let [[x-uncare y] coord]
    (loop [st #{}
           x 0]
      (if (>= x 9)
        st
        (recur (conj st (value-at board [x y])) (inc x))))))

(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

; Far too messy and I'm sure it's not clojure-ish, but functional
(defn block-values [board coord]
  (let [[x y] coord
        block-start (fn [z] (* 3 (int (/ z 3))))
        [startx starty] [(block-start x) (block-start y)]
        xs (map (fn [a] (+ a startx)) [0 1 2])
        ys (map (fn [b] (+ b starty)) [0 1 2])]
    (loop [st #{}
           crds (for [x xs
                      y ys]
                  [x y])]
      (let [f (first crds)
            r (rest crds)]
        (if (empty? f)
          st
          (recur (conj st (value-at board f)) r))))))


(defn valid-values-for [board coord]
  (if (== 0 (value-at board coord))
    (let [r (row-values board coord)
          c (col-values board coord)
          b (block-values board coord)]
      (set/difference all-values (set/union r c b)))
    #{}))


(defn filled? [board]
  (let [all-board-vals 
          (loop [st #{}
                 crds (coord-pairs [0 1 2 3 4 5 6 7 8])]
            (if (empty? crds)
              st
              (recur (conj st (value-at board (first crds))) (rest crds))))]
    (not (contains? all-board-vals 0))))

(defn rows [board]
  (loop [r 0
         lst []]
    (if (>= r 9)
      lst
      (recur (inc r) (concat lst [(row-values board [r 0])])) )))
;#{1 2 3})

; Lots of commonality between the valid-X? functions, rewrote out for
; simplicity of writing the rest
(defn AND [x y]
  (and x y))
(defn valid-set? [a-set]
  (= a-set all-values))
(defn valid-section? [board get-fn]
  (reduce AND (map valid-set? (get-fn board))))

(defn valid-rows? [board]
  ;(let [valid-row? (fn [row] (= row all-values))]
    ;(reduce AND (map valid-set? (rows board))))
  (valid-section? board rows))

(defn cols [board]
  (loop [c 0
         lst []]
    (if (>= c 9)
      lst
      (recur (inc c) (concat lst [(col-values board [0 c])])))))

(defn valid-cols? [board]
  (valid-section? board cols))

(defn blocks [board]
  ;block-values board coords
  (map (fn [z] (block-values board z))
    (for [x [0 3 6]
          y [0 3 6]]
      [x y])))

(defn valid-blocks? [board]
  (valid-section? board blocks))

(defn valid-solution? [board]
  (and (valid-rows? board) (valid-cols? board) (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (loop [coord (for [x [0 1 2 3 4 5 6 7 8]
                     y [0 1 2 3 4 5 6 7 8]]
                 [x y])]
    (if (empty? coord)
      nil
      (if (= 0 (value-at board (first coord)))
        (first coord)
        (recur (rest coord))))))

; TODO
(defn solve [board]
  nil)

