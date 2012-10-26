(ns sudoku.core
  (:require [clojure.set :as set]))

(def board identity)

(defn value-at [board coord]
   (get-in board coord)
)

(defn has-value? [board coord]
   (> (get-in board coord) 0)
)

(defn row-values [board coord]
   (set (get board (get coord 0)))
)

(defn col-values [board coord]
   (loop [a-set #{}
          a-board board
          n (get coord 1)]
       (if (empty? a-board)
           a-set
           (recur (conj a-set (get (first a-board) n))
                  (rest a-board) 
                  n)
       )
   )
)

(defn coord-pairs [coords]
 (vec (for [col1 coords
         col2 coords]
     [col1 col2]
   ))
)

(defn block-values [board coord]
  (defn block-helper1 [a-seq]
    (let [b-seq
      (for [row a-seq
           col a-seq]
        (if (and (= (mod row 3) 0)
                (= (mod col 3) 0))
          [row col]
        )
      )]
      (loop [c-seq []
             d-seq b-seq]
         (if (empty? d-seq)
           c-seq
           (if (= (first d-seq) nil)
             (recur c-seq (rest d-seq))
             (recur (conj c-seq (first d-seq)) (rest d-seq))
           )
         )
      )
    )
  )

  (defn block-helper2 [a-seq coord]
     (loop [seq1 []
            seq2 a-seq
            row (get coord 0)
            col (get coord 1)]
       (if (empty? seq2)
          seq1
          (if (and (<= (get (first seq2) 0) row (+ (get (first seq2) 0) 2))
                   (<= (get (first seq2) 1) col (+ (get (first seq2) 1) 2)))
            (concat seq1 (first seq2))
            (recur seq1 (rest seq2) row col)
          )
       )
     )
  )

  (defn block-helper3 [board coord]
     (let [a-seq
       (for [row [0 1 2]
           col [0 1 2]]
          (get-in board (conj [] (+ (get coord 0) row) (+ (get coord 1) col)))
       )]
       (set a-seq)
     )     
  )

  (block-helper3 board (vec (block-helper2 (block-helper1 (range 0 9)) coord)))
)

(defn valid-values-for [board coord]
  (if (> (get-in board coord) 0)
    #{}
    (set/difference #{1 2 3 4 5 6 7 8 9} 
                   (block-values board coord)
                   (row-values board coord)
                   (col-values board coord)
    )
  )
)

(defn filled? [board]
   (defn filled-helper [a-set board]
      (if (empty? board)
          a-set
          (filled-helper (set/union a-set (set (first board))) (rest board))
      )
   )
  (not (contains? (filled-helper #{} board) 0))
)

(defn rows [board]
   (defn rows-helper [a-seq board]
      (if (empty? board)
         a-seq
         (rows-helper (conj a-seq (set (first board))) (rest board))
      )
   )
  (rows-helper [] board)
)

(defn valid-rows? [board]
   (defn valid-helper [a-seq]
      (cond
         (empty? a-seq) true
         (< (count (first a-seq)) 9) false
         :else (valid-helper (rest a-seq))
      )
   )
  (valid-helper (rows board))
)

(defn cols [board]
     (loop [i 0
            a-seq []]
       (if (= i 9)
         a-seq
         (recur (inc i) (conj a-seq (col-values board (conj [] 0 i))))
       )
     )
)

(defn valid-cols? [board]
  (valid-helper (cols board))
)

(defn blocks [board]
   (defn blocks-helper1 [a-seq b-seq board]
      (if (empty? b-seq)
         a-seq
        (blocks-helper1 
          (conj a-seq (block-values board (first b-seq)))
          (rest b-seq)
           board
        )
      )

   )
   (blocks-helper1 [] (block-helper1 (range 0 9)) board)
)

(defn valid-blocks? [board]
   (valid-helper (blocks board))
)

(defn valid-solution? [board]
   (and (valid-rows? board)
        (valid-cols? board)
        (valid-blocks? board)
   )
)

(defn set-value-at [board coord new-value]
     (assoc-in board coord new-value)  
)

(defn find-empty-point [board]
 (first
  (let [b-seq
    (for [row [0 1 2 3 4 5 6 7 8]
         col [0 1 2 3 4 5 6 7 8]]
     (if (= (get-in board [row col]) 0)
        [row col]
     )
    )]
  (loop [c-seq []
         d-seq b-seq]
         (if (empty? d-seq)
           c-seq
           (if (= (first d-seq) nil)
             (recur c-seq (rest d-seq))
             (recur (conj c-seq (first d-seq)) (rest d-seq))
           )
         )
  )))
)

(defn solve [board]
  (defn solve-helper [board]
     (let [seq1 (find-empty-point board)]
         (if (filled? board)
	       (if (valid-solution? board)
				[board]
		   )
		   (for [v (valid-values-for board seq1)
				a-board (solve-helper (set-value-at board seq1 v))]
	     			a-board
           )
         )
     )
  )
  (first (solve-helper board))
)