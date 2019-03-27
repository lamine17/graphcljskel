(ns graphclj.centrality
    (:require [graphclj.graph :as graph]
              [clojure.set :as set]))



(defn degrees [g]
  (loop[ng {}
        ag g]
    (if(seq ag)
      (recur (assoc ng (first (first ag)) (assoc (second (first ag)) :degree (count (get (second (first ag)) :neigh)))) (rest ag))
      ng)))
      
  
(defn getallnoeud [g e]
  (loop[ne e
        s #{}]
    (if(seq ne)
      (recur (rest ne) (clojure.set/union s (get (get g (first ne)) :neigh)))
      s)))
      

(defn ajnoeud [m l t]
  (loop[m m
        nl l]
    (if(seq nl)
      (if(contains? m (first nl))
        (recur m  (rest nl))
        (recur (assoc m (first nl) t) (rest nl)))
      m)))

(defn distance [g n]
  (loop[m (assoc {} n 0.0)
        t 1.0
        e (set (list n))]
    (if(< (count e) (count g))
      (let[l (getallnoeud g e)]
        (recur (ajnoeud m l t) (+ t 1) (clojure.set/union e l)))
      m)))
        

(defn closeness [g n]
  (reduce + (map (fn [x ] (if (== x 0) 0 (float (/ 1 x)))) (vals (distance g n)))))


(defn closeness-all [g]
  (loop[ng {}
        ag g]
    (if(seq ag)
      (recur (assoc ng (first (first ag)) (assoc (second (first ag)) :close (closeness g (first (first ag))))) (rest ag))
      ng)))
