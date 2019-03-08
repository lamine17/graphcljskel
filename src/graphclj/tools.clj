(ns graphclj.tools
  (:require [clojure.string :as str]))

(defn readfile [f]
    "Returns a sequence from a file f"
    (with-open [rdr (clojure.java.io/reader f)]
            (doall (line-seq rdr))))

(defn rank-nodes [g,l]
  "Ranks the nodes of the graph in relation to label l in accending order"
  )

(defn generate-colors [n]
    (let [step 10]
    (loop [colors {}, current [255.0 160.0 122.0], c 0]
      (if (= c (inc n))
        colors
        (recur (assoc colors c (map #(/ (mod (+ step %) 255) 255) current))
               (map #(mod (+ step %) 255) current) (inc c))
      ))))



(defn to-dot [g]
  "Returns a string in dot format for graph g, each node is colored in relation to its ranking"
  )
