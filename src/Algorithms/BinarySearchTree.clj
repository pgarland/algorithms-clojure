(ns Algorithms.BinarySearchTree)

(defrecord BinarySearchTree [val left right])

(defn inorder-tree-walk [t node-fn]
  (when t
    (do (inorder-tree-walk (:left t) node-fn)
        (node-fn t)
        (inorder-tree-walk (:right t) node-fn))))

(defn preorder-tree-walk [t node-fn]
  (when t
    (do (node-fn t)
        (inorder-tree-walk (:left t) node-fn)
        (inorder-tree-walk (:right t) node-fn))))

(defn postorder-tree-walk [t node-fn]
  (when t
    (do (inorder-tree-walk (:left t) node-fn)
        (inorder-tree-walk (:right t) node-fn)
        (node-fn t))))

(defn print-tree [t walk-fn]
  (walk-fn t #(println (:val %))))

(defn search [t val]
  (cond (or (= t nil) (= (:val t) val)) t
        (< val (:val t))
        (search (:left t) val)
        :else (search (:right t) val)))

