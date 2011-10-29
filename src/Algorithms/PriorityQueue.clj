(ns Algorithms.PriorityQueue
  (:use [Algorithms.heap]))

(defprotocol PriorityQueue
  (insert [Q val])
  (peek [Q])
  (pop [Q])
  (set-val [Q idx val]))

(defrecord MaxPriorityQueue [heap])

(extend-type MaxPriorityQueue
  PriorityQueue
  (insert [Q val]
    (MaxPriorityQueue. (max-heap-insert (:heap Q val))))
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
    (MinPriorityQueue. (min-heap-insert (:heap Q val))))
  (peek [Q]
    (heap-min (:heap Q)))
  (pop [Q]
    (MinPriorityQueue. (heap-extract-min (:heap Q))))
  (set-val [Q idx val]
    (MinPriorityQueue. (heap-decrease-key (:heap Q) idx val))))