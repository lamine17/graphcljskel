(ns graphclj.core
  (:require [graphclj.graph :as graph]
            [graphclj.tools :as tools]
            [graphclj.centrality :as central])
  (:gen-class))

(defn -main
  [& args]
  (tools/to-dot (tools/rank-nodes (central/closeness-all (central/degrees (graph/gen-graph (tools/readfile (first args))))) (second args))))


(defn -main1
  [& args]
  (central/closeness-all (central/degrees (graph/gen-graph (tools/readfile (first args))))))
