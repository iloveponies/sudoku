(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)


; EXERCISE 1
(defn value-at [board coord]
  (get-in board coord))


; EXERCISE 2
(defn has-value? [board coord]
  (if (= (value-at board coord) 0)
    false
    true))


; EXERCISE 3
(defn row-values [board coord]
  (let [[r c] coord]
    (set (get board r))))


; EXERCISE 4
(defn col-values [board coord]
  (let [[r c] coord]
    (loop [acc #{}
           n 0]
      (let [v (value-at board [n c])]
        (cond (nil? v) acc
              :else (recur (conj acc v) (inc n)))))
    ))


; EXERCISE 5
(defn coord-pairs [coords]
  (for [a coords
        b coords]
    (seq [a b])))

;(coord-pairs [0 1])   ;=> [[0 0] [0 1]
;                      ;    [1 0] [1 1]]

;(coord-pairs [0 1 2]) ;=> [[0 0] [0 1] [0 2]
;                      ;    [1 0] [1 1] [1 2]
;                      ;    [2 0] [2 1] [2 2]]


; EXERCISE 6
(defn coord-apu [coord a b]
  (let [[r c] coord
        rn (+ r a)
        cn (+ c b)]
    [rn cn]))

(defn block-values [board coord]
  (let [[r c] coord
        a (* (int (/ r 3)) 3)
        b (* (int (/ c 3)) 3)
        coords (coord-pairs [0 1 2])]
    (loop [acc #{}
           r coords]
      (let [crd (first r)
            v (if (not (nil? crd)) (value-at board (coord-apu crd a b)))]
        (cond (empty? r) acc
              :else (recur (conj acc v) (rest r)))))))


; EXERCISE 7
(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
    (let [r (row-values board coord)
          c (col-values board coord)
          b (block-values board coord)
          u (set/union r c b)
          v (set (range 10))]
      (set/difference v u))))


; EXERCISE 8
(defn filled? [board]
  (let [coords (coord-pairs (range 9))
        hasvalues (map (fn [x] (has-value? board x)) coords)]
    (every? true? hasvalues)))


; EXERCISE 9, 10 ja 11
(defn rows [board]
  (map (fn [i] (row-values board [i i])) (range 9)))

; Apuja mahdollisille arvoille
(def defran (set (range 1 10)))

(defn valid-apu [board z ran]
  (let [hasvalues (map (fn [x] (every? (fn [y] (contains? x y)) ran)) z)]
    (every? true? hasvalues)))

(defn valid-rows? [board]
  (valid-apu board (rows board) defran))

(defn cols [board]
  (map (fn [i] (col-values board [i i])) (range 9)))

(defn valid-cols? [board]
  (valid-apu board (cols board) defran))

(defn blocks [board]
  (map (fn [i] (block-values board [i (* 3 (mod i 3))])) (range 9)))

(defn valid-blocks? [board]
  (valid-apu board (blocks board) defran))


; EXERCISE 12
(defn valid-solution? [board]
  (and (valid-cols? board)
       (valid-rows? board)
       (valid-blocks? board)))


; EXERCISE 13
(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))


; EXERCISE 14
(defn find-empty-point [board]
  (let [coords (coord-pairs (range 9))
        hasvalues (map (fn [x] (has-value? board x)) coords)]
    (loop [c coords
           v hasvalues]
      (cond (empty? c) nil
            (not (first v)) (first c)
            :else (recur (rest c) (rest v))
            ))))


; EXERCISE 15
(defn solve [board]
  (if (valid-solution? board)
    board
    (let [coord (find-empty-point board)
          remaining (valid-values-for board coord)]
      (for [c remaining
            solution (solve     (set-value-at board coord c))]
        solution))))
