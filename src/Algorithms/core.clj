(ns Algorithms.core)

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
  ([heap root-index test-fn] (or (> root-index (count heap))
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
  "Create a heap from the array and function given"
  (loop [array a
         index (dec (quot (count array) 2))]
    (if (> index 0)
      (let [new-index (dec index)]
        (recur (heapify-fun array new-index) new-index))
      array)))

(defn build-max-heap [a]
  "Create a max heap from the array given"
  (build-heap a max-heapify))

(defn build-min-heap [a]
  "Create a max heap from the array given"
  (build-heap a min-heapify))

