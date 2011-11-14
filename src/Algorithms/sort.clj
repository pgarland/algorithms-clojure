(ns Algorithms.sort)

(defn insertion-sort
  ([a] (insertion-sort [] a))
  ([l r] (if (not (seq r))
           l
           (recur (insert (first r) l) (rest r)))))

(defn insert
  ([elt a] (insert elt [] a))
  ([elt l r]) (if (or (not (seq r))
                      (<= elt (first r)))
                (reduce into [l [elt] r])
                  