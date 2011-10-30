(ns Algorithms.BinarySearchTree)

(defrecord BinarySearchTree [val left right])

(defn walk-tree-inorder [t node-fn]
  (when t
    (do (walk-tree-inorder (:left t) node-fn)
        (node-fn t)
        (walk-tree-inorder (:right t) node-fn))))

(defn walk-tree-post-order [t node-fn]
  (when t
    (do (node-fn t)
        (walk-tree-preorder (:left t) node-fn)
        (walk-tree-preorder (:right t) node-fn))))

(defn walk-tree-postorder [t node-fn]
  (when t
    (do (walk-tree-postorder (:left t) node-fn)
        (walk-tree-postorder (:right t) node-fn)
        (node-fn t))))

(defn print-tree [t walk-fn]
  (walk-fn t #(println (:val %))))

(defn search [t val]
  (cond (or (= t nil) (= (:val t) val)) t
        (< val (:val t))
        (search (:left t) val)
        :else (search (:right t) val)))

