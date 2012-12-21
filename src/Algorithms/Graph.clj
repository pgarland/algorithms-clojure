(ns Algorithms.Graph)

(defrecord Vertex [val])
(defrecord Edge [start end weight])
(defrecord AdjacencyList [vertices edges al])

(defprotocol Graph
  (in-degree [v G])
  (out-degree [v G])
  (bfs [G root Q visitf vals]))

(extend-type AdjacencyList
  Graph
  (out-degree [vtx graph]
    (count (graph vtx)))
  (in-degree [vtx graph]
    (count (for [v graph :when (some #{(:end vtx)} v)] v)))
  (bfs [G vtx Q visitf vals]
    ;;; Recursive implemtation of breadth-first search
    (letfn [(bfs [G vtx Q visitf vals]
              (let [adj ((:al G) vtx)]
                (if (and (empty? adj)
                         (empty? Q))
                  (conj vals (visitf (:val vtx)))
                  (let [Q (into Q adj)]
                    (recur G (peek Q) (pop Q) visitf (conj vals (visitf (:val vtx))))))))]
      (bfs G vtx Q visitf vals))))