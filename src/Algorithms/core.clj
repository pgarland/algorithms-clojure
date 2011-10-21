(ns Algorithms.core)

(defn parent-index [index]
  (quot (dec index) 2))

(defn left-index [index]
  (* 2 (inc index)))

(defn right-index [index]
  (+ (* (inc index) 2) 1))

(defn test-heap
  ([heap test-fn] (and (test-heap heap (left-index 0) test-fn)
                       (test-heap heap (right-index 0) test-fn)))
  ([heap root-index test-fn] (or (> root-index (count heap))
                                 (and (test-fn heap root-index)
                                      (test-heap heap (left-index root-index) test-fn)
                                      (test-heap heap (right-index root-index) test-fn)))))


(defn max-heap? [heap]
  (test-heap heap
             (fn [heap root-index] (>= (nth heap (parent-index root-index))
                                       (nth heap root-index)))))


(defn min-heap? [heap]
  (test-heap heap
             (fn [heap root-index] (<= (nth heap (parent-index root-index))
                                       (nth heap root-index)))))