(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)

(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
 (get-in board coord))

;; (value-at sudoku-board [0 1]) ;=> 3
;; (value-at sudoku-board [0 0]) ;=> 5

(defn has-value? [board coord]
  ((complement = )0  (value-at board coord)))

;; (has-value? sudoku-board [0 0]) ;=> true
;; (has-value? sudoku-board [0 2]) ;=> false

(defn row-values [board coord]
  (let [[ row] coord]
   (set
    (get board  row))))

;;  (row-values sudoku-board [0 2]) ;=> #{0 5 3 7}
;;  (row-values sudoku-board [3 2]) ;=> #{0 8 6 3}

(defn col-values [board coord]
  (set (let [  col (second coord)]
    (for [ i (range (count board))]  (get-in board [i col])))))


;;  (col-values sudoku-board [0 2]) ;=> #{0 8}
;;  (col-values sudoku-board [4 8]) ;=> #{3 1 6 0 5 9}



;; (defn coord-pairs [coords]
;;   (loop [ liste coords c []]
;;     (cond
;;       (empty? liste) c
;;       :else (recur (rest liste)
;;                    (concat
;;                      c
;;                      (map #(conj [(first liste)] % )coords))))))

(defn coord-pairs [coords]
  (for [ i coords j coords ] [i j]))

;; (coord-pairs [0 1])   ;=> [[0 0] [0 1]
;;                       ;    [1 0] [1 1]]
;; (coord-pairs [0 1 2]) ;=> [[0 0] [0 1] [0 2]
;;                       ;    [1 0] [1 1] [1 2]
;;                       ;    [2 0] [2 1] [2 2]]

(defn corner [num-row num-col]
  (let [row-corner (* 3 num-row)
        col-corner (* 3 num-col)]
    (vector row-corner col-corner)))

;; (corner 0 0) ;=>[ 0 0 ]
;; (corner 1 1) ;=>[ 3 3 ]
;; (corner 2 2) ;=>[ 6 6 ]

(defn add-point [point1 point2]
  (map #(apply + %) (map vector point1 point2)))

;;(add-point [0 1] [1 2]);=>(1 3)


(defn translate-block [board coord-corner]
  (let [ size-board (count board)
         block (coord-pairs [0 1 2])
         [corner-row corner-col] coord-corner ]
    (for [ [i j] block
           :let [x (+ i corner-row)
                 y (+ j corner-col)] ]
      [ x y ])))


;; (def block (coord-pairs [0 1 2]))
;; (def ex-corner [ 3 3])
;; (def block-translate (map #(add-point ex-corner % ) block))
;; (= block-translate (translate-block sudoku-board [3 3])) ;=> true


(defn block-values [board coord]
  (let [ [row col] coord
         block-row (int (/ row 3))
         block-col (int (/ col 3))
         coord-corner (corner block-row block-col)
         new-block  (translate-block board  coord-corner )]
    (set (map #(get-in board %) new-block))))


;;   (block-values sudoku-board [0 2]) ;=> #{0 5 3 6 8 9}
;;   (block-values sudoku-board [4 5]) ;=> #{0 6 8 3 2}


(defn valid-values-for [board coord]
  (let [ [col row ] coord
         col-val   (col-values board coord)
         row-val   (row-values board coord)
         block-val (block-values board coord)]
  (cond
    (= 0 (get-in board coord))(clojure.set/difference all-values
                                (clojure.set/union  col-val
                                                    row-val
                                                    block-val))
    :else #{})))


;;   (valid-values-for sudoku-board [0 0]) ;=> #{}
;;   (valid-values-for sudoku-board [0 2]) ;=> #{1 2 4}

(defn full-block? [board corner]
  (empty? (valid-values-for board corner)))



(defn filled? [board]
  (reduce (fn [c x]
            (and c x))
          true
          (for [i [1 2 3] j [1 2 3]]
       (full-block? board [i j]))))


;; (filled? sudoku-board);=> false
;; (filled? solved-board) ;=> true


(defn rows [board]
  (for [ row  (range (count board))]
    (row-values board [ row 0])))



;; (rows sudoku-board) ;=> [#{5 3 0 7}
;;                     ;    #{6 0 1 9 5}
;;                     ;    #{0 9 8 6}
;;                     ;    #{8 0 6 3}
;;                     ;    #{4 0 8 3 1}
;;                     ;    #{7 0 2 6}
;;                     ;    #{0 6 2 8}
;;                     ;    #{0 4 1 9 5}
;;                     ;    #{0 8 7 9}]

;; (rows solved-board) ;=> [#{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}]

(defn valid-rows? [board]
  (let [ size-board (count board) ]
       (= size-board
          (count  (for [ row (rows board) :when (= (count row ) size-board)] 1)))))


;; (valid-rows? solved-board)  ;=> truthy
;; (valid-rows? sudoku-board) ;=> falsey


(defn cols [board]
 (for [ col (range(count board))]
   (col-values board [ 0 col ])))

;; (cols sudoku-board) ;=> [#{5 6 0 8 4 7}
;;                     ;    #{3 0 9 6}
;;                     ;    #{0 8}
;;                     ;    #{0 1 8 4}
;;                     ;    #{7 9 0 6 2 1 8}
;;                     ;    #{0 5 3 9}
;;                     ;    #{0 2}
;;                     ;    #{0 6 8 7}
;;                     ;    #{0 3 1 6 5 9}]

;; (cols solved-board) ;=> [#{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}
;;                     ;    #{1 2 3 4 5 6 7 8 9}]


(defn valid-cols? [board]
  (let [ size-board (count board) ]
       (= size-board
          (count  (for [ col(cols board) :when (= (count col ) size-board)] 1)))))

;; (valid-cols? solved-board)  ;=> truthy
;; (valid-cols? sudoku-board) ;=> falsey


(defn blocks [board]
   (for [ i (range 0 7 3) j (range 0 7 3) ]
     (block-values board [ i j])))

;; (blocks sudoku-board) ;=> [#{5 3 0 6 9 8}
;;                       ;    #{0 7 1 9 5}
;;                       ;    #{0 6}
;;                       ;    #{8 0 4 7}
;;                       ;    #{0 6 8 3 2}
;;                       ;    #{0 3 1 6}
;;                       ;    #{0 6}
;;                       ;    #{0 4 1 9 8}
;;                       ;    #{2 8 0 5 7 9}]

;; (blocks solved-board) ;=> [#{1 2 3 4 5 6 7 8 9}
;;                       ;    #{1 2 3 4 5 6 7 8 9}
;;                       ;    #{1 2 3 4 5 6 7 8 9}
;;                       ;    #{1 2 3 4 5 6 7 8 9}
;;                       ;    #{1 2 3 4 5 6 7 8 9}
;;                       ;    #{1 2 3 4 5 6 7 8 9}
;;                       ;    #{1 2 3 4 5 6 7 8 9}
;;                       ;    #{1 2 3 4 5 6 7 8 9}
;;                       ;    #{1 2 3 4 5 6 7 8 9}])

(defn valid-blocks? [board]
  (let [ size-board (count board) ]
       (= size-board
          (count  (for [ block (blocks board) :when (= (count block ) size-board)] 1)))))

;; (valid-blocks? solved-board)  ;=> truthy
;; (valid-blocks? sudoku-board) ;=> falsey


(defn valid-solution? [board]
  (and (valid-blocks? board)
       (valid-rows? board)
       (valid-cols? board)))


;;  (valid-solution? solved-board)  ;=> truthy
;;  (valid-solution? sudoku-board) ;=> falsey


(defn set-value-at [board coord new-value]
    (assoc-in board coord new-value))


;; (def before-change
;;   (board [[5 3 0 0 7 0 0 0 0]
;;           [6 0 0 1 9 5 0 0 0]
;;           [0 9 8 0 0 0 0 6 0]
;;           [8 0 0 0 6 0 0 0 3]
;;           [4 0 0 8 0 3 0 0 1]
;;           [7 0 0 0 2 0 0 0 6]
;;           [0 6 0 0 0 0 2 8 0]
;;           [0 0 0 4 1 9 0 0 5]
;;           [0 0 0 0 8 0 0 7 9]]))

;; (def after-change
;;   (board [[5 3 0 0 7 0 0 0 0]
;;           [6 0 0 1 9 5 0 0 0]
;;           [0 4 8 0 0 0 0 6 0]
;;           [8 0 0 0 6 0 0 0 3]
;;           [4 0 0 8 0 3 0 0 1]
;;           [7 0 0 0 2 0 0 0 6]
;;           [0 6 0 0 0 0 2 8 0]
;;           [0 0 0 4 1 9 0 0 5]
;;           [0 0 0 0 8 0 0 7 9]]))

;; (= (set-value-at before-change [2 1] 4)
;;     after-change)


(defn pos-0 [line ]
  (let [ size (count line)]
    (for [ i (range size)
           :when (= 0
                    (get line i))]
      i)))

(defn find-empty-point [board]
  (let [ size (count board)
         set-points (for [ i (range size)
                            j (range size)
                            :when (= 0 (get-in board [i j]))]
                       [i j] )
         number-points (count set-points)
         index (rand-int number-points)]
     (get  (apply vector set-points) index)))




(defn solve [board]
 [:=])


;;   (def sudoku-board
;;   (board [[5 3 0 0 7 0 0 0 0]
;;           [6 0 0 1 9 5 0 0 0]
;;           [0 9 8 0 0 0 0 6 0]
;;           [8 0 0 0 6 0 0 0 3]
;;           [4 0 0 8 0 3 0 0 1]
;;           [7 0 0 0 2 0 0 0 6]
;;           [0 6 0 0 0 0 2 8 0]
;;           [0 0 0 4 1 9 0 0 5]
;;           [0 0 0 0 8 0 0 7 9]]))

;; (def solved-board
;;   (board [[5 3 4 6 7 8 9 1 2]
;;           [6 7 2 1 9 5 3 4 8]
;;           [1 9 8 3 4 2 5 6 7]
;;           [8 5 9 7 6 1 4 2 3]
;;           [4 2 6 8 5 3 7 9 1]
;;           [7 1 3 9 2 4 8 5 6]
;;           [9 6 1 5 3 7 2 8 4]
;;           [2 8 7 4 1 9 6 3 5]
;;           [3 4 5 2 8 6 1 7 9]]))


