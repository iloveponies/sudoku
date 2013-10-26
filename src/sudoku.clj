(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
  (get-in board coord))


(defn has-value? [board coord]
  (not (= (value-at board coord) 0)))


(defn row-values [board coord]
  (let [[y x] coord]
    (set (board y))))


(defn col-values [board coord]
  (let [[y x] coord]
    (loop [i 0
           values (set '())]
      (cond
       (= i 8) (conj values (get-in board [i x]))
       :else (recur (inc i) (conj values (get-in board [i x])))))))


(defn coord-pairs [coords]
  nil)

(defn block-values [board coord]
  nil)

(defn valid-values-for [board coord]
  nil)


(defn filled? [board]
    (loop [i 0
           values '()]
      (cond
       (= i 8) (not (contains? (set (concat values (row-values board [i 0]))) 0))
       :else (recur (inc i) (concat values (row-values board [i 0]))))))


(defn rows [board]
  (loop [i 0
           values []]
      (cond
       (= i 8) (conj values (row-values board [i 0]))
       :else (recur (inc i) (conj values (row-values board [i 0]))))))

(defn valid-rows? [board]
  nil)

(defn cols [board]
  (loop [i 0
         values []]
      (cond
       (= i 8) (conj values (col-values board [0 i]))
       :else (recur (inc i) (conj values (col-values board [0 i]))))))

(defn valid-cols? [board]
  nil)

(defn blocks [board]
  nil)

(defn valid-blocks? [board]
  nil)

(defn valid-solution? [board]
  nil)

(defn set-value-at [board coord new-value]
  nil)

(defn find-empty-point [board]
  (let [
     ;; sudokun luvut listana
     numbers (loop [i 0
                    values []]
       (cond
         (= i 8) (concat values (get board i))
         :else (recur (inc i) (concat values (get board i)))))

     ;;Haetaan listasta monesko on nolla
     findI (loop[i 0
                numbs numbers]
       (cond
         (= i 82) nil
         (= (first numbs) 0) i
         :else (recur (inc i) (rest numbs))))

      ;;Lasketaan y arvo
     getY (fn [n] (int (/ n 8)))]

  (cond
    (= findI nil) nil
    :else [(getY findI) (- findI (* 9 (getY findI)))])))


(defn solve [board]
  nil)
