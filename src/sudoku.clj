(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

;E1
(defn value-at [board coord]
  (get-in board coord))

;E2
(defn has-value? [board coord]
  (if (= (value-at board coord) 0)
    false
    true))

;E3
(defn row-values [board coord]
  (let [[r c] coord]
    (set (get board r))))
;E4
(defn col-values [board coord]
  (let [[r c] coord]
    (loop [acc #{}
           n 0]
      (let [v (value-at board [n c])]
        (cond (nil? v) acc
              :else (recur (conj acc v) (inc n)))))
    ))

;E5
(defn coord-pairs [coords]
  (for [a coords
        b coords]
    (seq [a b])))
; (coord-pairs [1 2])
; (seq [1 2])
;E6
(defn coord-helper [coord a b]
  (let [[r c] coord
        rn (+ r a)
        cn (+ c b)]
    [rn cn]))
; (coord-helper [1 2] 1 0)
(defn block-values [board coord]
  (let [[r c] coord
        a (* (int (/ r 3)) 3)
        b (* (int (/ c 3)) 3)
        coords (coord-pairs [0 1 2])]
    (loop [acc #{}
           r coords]
      (let [crd (first r)
            v (if (not (nil? crd)) (value-at board (coord-helper crd a b)))]
        (cond (empty? r) acc
              :else (recur (conj acc v) (rest r)))))))
; (int (/ 8 3))
(disj #{1 11} (range 10))

;E7
(defn valid-values-for [board coord]
  (if (has-value? board coord) #{}
    (let [r (row-values board coord)
          c (col-values board coord)
          b (block-values board coord)
          u (set/union r c b)
          v (set (range 10))]
      (set/difference v u))))

;E8
(defn filled? [board]
  (let [coords (coord-pairs (range 9))
        hasvalues (map (fn [x] (has-value? board x)) coords)]
    (every? true? hasvalues)))

;E9-11
(defn rows [board]
    (map (fn [i] (row-values board [i i])) (range 9)))


;Apu, luvut 1...9, eli mahdolliset arvot
(def defran (set (range 1 10)))

(defn valid-helper [board z ran]
  (let [hasvalues (map (fn [x] (every? (fn [y] (contains? x y)) ran)) z)]
    (every? true? hasvalues)))

(defn valid-rows? [board]
  (valid-helper board (rows board) defran)) ;false))

(defn cols [board]
  (map (fn [i] (col-values board [i i])) (range 9)))


(defn valid-cols? [board]
  (valid-helper board (cols board) defran))

(defn blocks [board]
  (map (fn [i] (block-values board [i (* 3 (mod i 3))])) (range 9)))


(defn valid-blocks? [board]
  (valid-helper board (blocks board) defran))

;E12
(defn valid-solution? [board]
  (and (valid-cols? board)
       (valid-rows? board)
       (valid-blocks? board)))

;E13
(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))


;E14 tsekkaa jos loppui jo yms
(defn find-empty-point [board]
  (let [coords (coord-pairs (range 9))
        hasvalues (map (fn [x] (has-value? board x)) coords)]
    (loop [c coords
           v hasvalues]
      (cond (empty? c) nil
            (not (first v)) (first c)
            :else (recur (rest c) (rest v))
            ))))


;E15
(defn solve [x]
 (if (valid-solution? x)
    x
    (let [coord (find-empty-point x)
          remaining (valid-values-for x coord)]
      (for [y remaining
            solution (solve     (set-value-at x coord y))]
        solution))))
