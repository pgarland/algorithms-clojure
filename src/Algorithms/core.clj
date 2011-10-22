(ns Algorithms.core)

(defn parent-index [index]
  "Return the parent index of current node"
  (quot (dec index) 2))

(defn left-index [index]
  "Return the index of the left node of the current node"
  (inc (* 2 index)))

(defn right-index [index]
  "Return the index of the left node of the current node"
  (* (inc index) 2))

(defn test-heap
  "Apply test-fn to every node in the heap"
  ([heap test-fn] (and (test-heap heap (left-index 0) test-fn)
                       (test-heap heap (right-index 0) test-fn)))
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

(defn max-heapify [array root]
  (let [left (left-index root)
        right (right-index root)
        valid-indices (filter #(< % (count array))
                              [root left right])
        max-index (apply (partial max-key array) valid-indices)]
    (print root valid-indices max-index)
    (if (not (= max-index root))
      (max-heapify (swap array root max-index) max-index)
      array)))
        