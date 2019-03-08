(ns graphclj.centrality
    (:require [graphclj.graph :as graph]
              [clojure.set :as set]))



(defn degrees [g]
  "Calculates the degree centrality for each node"
  )

(defn distance [g n]
  "Calculate the distances of one node to all the others")

(defn closeness [g n]
  "Returns the closeness for node n in graph g")


(defn closeness-all [g]
  "Returns the closeness for all nodes in graph g")
