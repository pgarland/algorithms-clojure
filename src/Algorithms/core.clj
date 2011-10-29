(ns Algorithms.core)

;; The algorithms are based on discussion and pseudocode in:
;; Introduction to Algorithms (3rd Edition)
;; Cormen TH, Leiserson CE, Rivest, RL, and Stein C
;; http://mitpress.mit.edu/algorithms/

;;; Code for dealing with heaps

(defn parent-index [index]
  "Return the parent index of current node"
  (quot (dec index) 2))

(defn left-index [index]
  "Return the index of the left child of the root"
  (inc (* 2 index)))

(defn right-index [index]
  "Return the index of the right child of the root"
  (* (inc index) 2))

(defn test-heap
  "Return true if test-fn is true of every node in the heap, false otherwise"
  ;;; If no index has given, we're at the root of the tree; test both branches
  ([heap test-fn] (and (test-heap heap (left-index 0) test-fn)
                       (test-heap heap (right-index 0) test-fn)))
  ;;; If given an index, check to see if we've gone past the bottom of
  ;;; the tree. If we haven't, test the root, as well as the left and right branches
  ([heap root-index test-fn]
     (println root-index)
     (println (count heap))
     (println)
     (or (> root-index (dec (count heap)))
         (and (test-fn heap root-index)
              (test-heap heap (left-index root-index) test-fn)
              (test-heap heap (right-index root-index) test-fn)))))


(defn max-heap? [heap]
  "Return true if heap is a max heap, false otherwise"
  (test-heap heap
             (fn [heap root-index] (>= (nth heap (parent-index root-index))
                                       (nth heap root-index)))))


(defn min-heap? [heap]
  "Return true if heap is a min heap, false otherwise"
  (test-heap heap
             (fn [heap root-index] (<= (nth heap (parent-index root-index))
                                       (nth heap root-index)))))

(defn swap [a i j]
  "Return a vector where the values in indices i and j have been swapped"
  (assoc a i (a j) j (a i)))


;;; heapify is used to implement max-heapify and min-heapify by
;;; calling heapify with select-fun as max-key and min-key,
;;; respectively
(defn heapify [array root select-fun]
  "Move the value from the root index of array into the index
determined by select fun. Assumes that the left and right branches of
the tree rooted at root are already ordered by select fun."
  (let [left (left-index root)
        right (right-index root)
        valid-indices (filter #(< % (count array))
                              [root left right])
        selected-index (apply (partial select-fun array) valid-indices)]
    (if (not (= selected-index root))
      (heapify (swap array root selected-index) selected-index select-fun) 
      array)))

(defn max-heapify [array root]
  "Move the value from the root index of array into the proper place
to make array a max heap. Assumes that the left and right branches the
tree rooted at root are valid max heaps."
  (heapify array root max-key))
  
(defn min-heapify [array root]
    "Move the value from the root index of array into the proper place
to make array a min heap. Assumes that the left and right branches the
tree rooted at root are valid min heaps."
  (heapify array root min-key))

(defn build-heap [a heapify-fun]
  "Create a heap from the vector and function given"
  (loop [array a
         index (quot (count array) 2)]
    (if (> index 0)
      (let [new-index (dec index)]
        (recur (heapify-fun array new-index) new-index))
      array)))

(defn build-max-heap [a]
  "Create a max heap from the vector given"
  (build-heap a max-heapify))

(defn build-min-heap [a]
  "Create a min heap from the vector given"
  (build-heap a min-heapify))

(defn heapsort [a]
  "Sort a into an increasing vector"
  (loop [heap (build-min-heap a)
         acc []]
         (if (= (count heap) 1)
           (conj acc (last heap))
           (let [heap-end (dec (count heap))
                 swapped-heap (swap heap 0 heap-end)
                 new-heap (subvec swapped-heap 0 heap-end)]
             (recur (min-heapify new-heap 0)
                    (conj acc (last swapped-heap)))))))

(defn heap-pop [heap heapify-fn]
  (let [last-idx (dec (count heap))
        new-heap (subvec (assoc heap 0 (heap last-idx)) 0 last-idx)]
    (heapify-fn new-heap 0)))

(defn heap-set-val [heap index key cmp-fn]
  (loop [new-heap (assoc heap index key)
         i index]
    ;; Walk up the heap from i, exchanging elements until it's restored to
    ;; being a valid heap, as ordered my cmp-fn
    (if (or (= i 0)
            (cmp-fn (new-heap (parent-index i))
                    (new-heap i)))
      new-heap
      (recur (swap new-heap i (parent-index i))
             (parent-index i)))))

(defn heap-max [heap]
  "Return the largest value from the heap"
  (heap 0))

(defn heap-extract-max [heap]
  "Remove the largest element from the heap, returning the rest as a max heap"
  (let [last-elt (dec (count heap))
        new-heap (subvec (assoc heap 0 (heap last-elt)) 0 last-elt)]
    (max-heapify new-heap 0)))

(defn heap-increase-key [heap index key]
  {:pre [(> key (heap index))]}
  (heap-set-val heap index key >))

(defn max-heap-insert [heap key]
  "Insert key into heap, maintaing the heap property"
  (let [new-heap (conj heap java.lang.Long/MIN_VALUE)]
    (heap-increase-key new-heap (dec (count new-heap)) key)))

(defn heap-min [min-heap]
  "Remove the smallest element from the heap, returning the rest as a min heap"
  (min-heap 0))

(defn heap-extract-min [heap]
  "Remove the largest element from the heap, returning the rest as a max heap"
  (heap-pop heap min-heapify))

(defn heap-decrease-key [heap index key]
  "Return the heap with the value in index set to key"
  {:pre [(< key (heap index))]}
  (heap-set-val heap index key <))

(defn min-heap-insert [heap key]
  "Insert key into heap, maintaing the heap property"
  (let [new-heap (conj heap java.lang.Long/MAX_VALUE)]
    (heap-increase-key new-heap (dec (count new-heap)) key)))

(defprotocol priority-queue
  (insert [Q val])
  (peek [Q])
  (pop [Q])
  (set-val [Q idx val]))

(defrecord max-priority-queue [heap])

(extend-type max-priority-queue
  priority-queue
  (insert [Q val]
    (max-priority-queue. (max-heap-insert (:heap Q val))))
  (peek [Q]
    (heap-max (:heap Q)))
  (pop [Q]
    (max-priority-queue. (heap-extract-max (:heap Q))))
  (set-val [Q idx val]
    (max-priority-queue. (heap-increase-key (:heap Q) idx val))))

(defrecord min-priority-queue [heap])

(extend-type min-priority-queue
  priority-queue
  (insert [Q val]
    (min-priority-queue. (min-heap-insert (:heap Q val))))
  (peek [Q]
    (heap-min (:heap Q)))
  (pop [Q]
    (min-priority-queue. (heap-extract-min (:heap Q))))
  (set-val [Q idx val]
    (min-priority-queue. (heap-decrease-key (:heap Q) idx val))))