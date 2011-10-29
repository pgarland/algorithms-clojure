(ns Algorithms.core)

;; The algorithms are based on discussion and pseudocode in:
;; Introduction to Algorithms (3rd Edition)
;; Cormen TH, Leiserson CE, Rivest, RL, and Stein C
;; http://mitpress.mit.edu/algorithms/

(defprotocol PriorityQueue
  (insert [Q val])
  (peek [Q])
  (pop [Q])
  (set-val [Q idx val]))

(defrecord MaxPriorityQueue [heap])

(extend-type MaxPriorityQueue
  PriorityQueue
  (insert [Q val]
    (MaxPriorityQueue. (Maxheap-insert (:heap Q val))))
  (peek [Q]
    (heap-max (:heap Q)))
  (pop [Q]
    (MaxPriorityQueue. (heap-extract-max (:heap Q))))
  (set-val [Q idx val]
    (MaxPriorityQueue. (heap-increase-key (:heap Q) idx val))))

(defrecord MinPriorityQueue [heap])

(extend-type MinPriorityQueue
  PriorityQueue
  (insert [Q val]
    (MinPriorityQueue. (Minheap-insert (:heap Q val))))
  (peek [Q]
    (heap-min (:heap Q)))
  (pop [Q]
    (MinPriorityQueue. (heap-extract-min (:heap Q))))
  (set-val [Q idx val]
    (MinPriorityQueue. (heap-decrease-key (:heap Q) idx val))))