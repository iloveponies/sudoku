(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})


          
(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (if (> (get-in board coord) 0)
     true
     false))

(defn row-values [board coord]
  (set (get board (get coord 0))))

(defn col-values [board coord]
  (let [apu (fn [c a] (conj c (get a (get coord 1))))]
    (set (reduce apu [] board))))

(defn coord-pairs [coords]
  (let [pairs (fn [c] 
                (for [x coords
                      y coords]
                   (concat c (vector x y))))]
    (pairs [])))

(defn top-left [coords]
  (vector (* 3 (int (/ (first coords) 3))) (* 3 (int (/ (second coords) 3)))))

(defn get-coords [bg]
  (let [sum (fn [c x] (concat c (vector (vector (+ (first bg) (first x)) (+ (second bg) (second x))))))]
   (reduce sum [] (coord-pairs [0 1 2]))))  

(defn block-values [board coord]
  (let [hae (fn [c x] (concat c (vector (get-in board x))))]
  (set (reduce hae [] (get-coords (top-left coord))))))

(defn valid-values-for [board coord]
  (if (has-value? board coord)
    (set ())
    (set/difference all-values (set/union (block-values board coord) (set/union (row-values board coord) (col-values board coord))))))

(defn board-to-vector [board]
  (let [assembly (fn [c x] (concat c x))]
  (set (reduce assembly [] board))))
  
(defn filled? [board]
  (if (contains? (board-to-vector board) 0)
    false
    true))

(defn rows [board]
  (let [collect (fn [c n] (conj c (row-values board (vector n 0))))]
  (reduce collect [] (range 0 9))))

(defn cols [board]
  (let [collect (fn [c n] (conj c (col-values board (vector 0 n))))]
  (reduce collect [] (range 0 9))))

(defn block-coordinates []
  (loop [x 0
         y 0
         c []]
         (if (and (= x 2) (= y 3))
           c
           (if (= y 3)
             (recur (inc x) 1 (concat c (vector (vector (* 3 (inc x)) 0))))
             (recur x (inc y) (concat c (vector (vector (* 3 x) (* 3 y)))))))))
               
 (defn blocks [board]
  (let [collect (fn [c x] (concat c (vector (block-values board x))))]
  (reduce collect [] (block-coordinates))))
  
(defn valid-rows? [board]
  (let [check (fn [r] (if (and (= (count r) (count (set r))) (not (contains? (set r) 0))) false true))]
    (loop [n 0]
      (if (= n (count board))
        true
        (if (check (get board n))
          false
          (recur (inc n)))))))

(defn pick-cols [board]
  (loop [n 0
         c []]
         (if (= n (count (first board)))
           c
           (recur (inc n) (concat c (vector (col-values board (vector 0 n))))))))

(defn valid-cols? [board]
  (loop [c (pick-cols board)]
         (if (empty? c)
           true
           (if (< (count (first c)) 9)
             false
             (recur (rest c))))))



(defn valid-blocks? [board]
  (loop [c (blocks board)]
     (if (empty? c)
       true
       (if (< (count (first c)) 9)
         false
         (recur (rest c))))))

(defn valid-solution? [board]
  (if (and (valid-rows? board) (valid-cols? board) (valid-blocks? board))
    true
    false))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
    (loop [x 0
           y 0]
      (if (> x 8)
        []
        (if (= (get-in board (vector x y)) 0)
          (vector x y)
        (if (> y 8)
          (recur (inc x) 0)
          (recur x (inc y)))))))
          
(defn empty-points [board]
    (loop [x 0
           y 0
           c []]
      (if (> x 8)
        c
        (if (> y 8)
          (if (= (get-in board (vector x y)) 0)
            (recur (inc x) 0 (concat c (vector (vector x y))))
            (recur (inc x) 0 c))
          (if (= (get-in board (vector x y)) 0)
            (recur x (inc y) (concat c (vector (vector x y))))
            (recur x (inc y) c))))))

(defn solve-helper [a-set board]
   (if (valid-solution? board)
      (vector board)
      (let [rem (valid-values-for board (first a-set))]
      (for [e rem
           result (solve-helper (rest a-set) (set-value-at board (first a-set) e))]
           result))))
           


(defn solve [board]
  (let [sol (solve-helper (empty-points board) board)]
    (if (valid-solution? (first sol))
       (first sol)
       [])))
  

