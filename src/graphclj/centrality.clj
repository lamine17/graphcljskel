(ns graphclj.centrality
    (:require [graphclj.graph :as graph]
              [clojure.set :as set])
  (:use midje.sweet))

;;La fonction degrees renvoie 
;;ajoute a chaque noeud d'un graph sa centralité

(defn degrees [g]
  (loop[ng {}
        ag g]
    (if(seq ag)
      (recur (assoc ng (first (first ag)) (assoc (second (first ag)) :degree (count (get (second (first ag)) :neigh)))) (rest ag))
      ng)))

(fact "La fonction degrees fonctionne correctement."      
    (let[g {1 {:neigh #{0 4 3}}
            0 {:neigh #{1 3}}
            3 {:neigh #{0 1 2}}
            4 {:neigh #{1}}
            2 {:neigh #{3}}}]
      (degrees g)
      =>
      {1 {:neigh #{0 4 3} :degree 3}
       0 {:neigh #{1 3} :degree 2}
       3 {:neigh #{0 1 2} :degree 3}
       4 {:neigh #{1} :degree 1}
       2 {:neigh #{3} :degree 1}}))
        
      

;; La fonction getallnoeud
;; renvoie un ensemble des liens auquels sont
;; liées les noeuds fournis dans une sequence
;; e en argument.

(defn getallnoeud [g e]
  (loop[ne e
        s #{}]
    (if(seq ne)
      (recur (rest ne) (clojure.set/union s (get (get g (first ne)) :neigh)))
      s)))


  
(fact "La fonction getallneoud fonctionne correctement."      
    (getallnoeud {0 {:neigh #{1}} 1 {:neigh #{0 2}} 2 {:neigh #{1}}} [0 2])
    =>
    #{1}
    (getallnoeud {0 {:neigh #{1}} 1 {:neigh #{0 2}} 2 {:neigh #{1}}} [1 2])
    =>
    #{0 1 2}
    (getallnoeud {0 {:neigh #{1}} 1 {:neigh #{0 2}} 2 {:neigh #{1}}} [])
    =>
    #{})
 
  
  
;;La fonction ajnoeud renvoie une sequence 
;;a laquelle si un membre de l'ensemble t
;;n'etait pas deja present dans l'ensemble des clés de la map m
;;en argument elle l'ajoute a cette ensemble m
;;puis le donne pour argument l'argument t.
;;La map m nouvellemnt formé est renvoyé.

(defn ajnoeud [m l t]
  (loop[m m
        nl l]
    (if(seq nl)
      (if(contains? m (first nl))
        (recur m  (rest nl))
        (recur (assoc m (first nl) t) (rest nl)))
      m)))

(fact "La fonction ajnoeud fonctionne correctement."      
    (ajnoeud {:a 1 :b 2 } #{:a :c} 3)
    =>
    {:a 1 :b 2 :c 3})

;;La fonction distance renvoie la sous forme 
;;d'un sequence la distance entre un noeud et les autres neouds du
;;graph

(defn distance [g n]
  (loop[m (assoc {} n 0.0)
        t 1.0
        e (set (list n))]
    (if(< (count e) (count g))
      (let[l (getallnoeud g e)]
        (recur (ajnoeud m l t) (+ t 1) (clojure.set/union e l)))
      m)))


(fact "La fonction distance fonctionne correctement."      
  (let[g {1 {:neigh #{0 4 3}}
          0 {:neigh #{1 3}}
          3 {:neigh #{0 1 2}}
          4 {:neigh #{1}}
          2 {:neigh #{3}}}]
    (distance g 1))
  =>
  {0 1.0, 4 1.0, 3 1.0, 1 0.0, 2 2.0})     

;;La fonction closseness renvoie la centralité 
;;d'un noeud n au sein d'un graph g.

(defn closeness [g n]
  (reduce + (map (fn [x ] (if (== x 0) 0 (float (/ 1 x)))) (vals (distance g n)))))



(fact "La fonction distance fonctionne correctement."      
  (let[g {1 {:neigh #{0 4 3}}
          0 {:neigh #{1 3}}
          3 {:neigh #{0 1 2}}
          4 {:neigh #{1}}
          2 {:neigh #{3}}}]
    (closeness g 1))
  =>
  3.5) 

;;La fonction closeness-all ajoute a chaque neoud d'un graphe
;;sa valeur de centralité.

(defn closeness-all [g]
  (loop[ng {}
        ag g]
    (if(seq ag)
      (recur (assoc ng (first (first ag)) (assoc (second (first ag)) :close (closeness g (first (first ag))))) (rest ag))
      ng)))


(fact "La fonction distance fonctionne correctement."      
  (let[g {1 {:neigh #{0 4 3}}
          0 {:neigh #{1 3}}
          3 {:neigh #{0 1 2}}
          4 {:neigh #{1}}
          2 {:neigh #{3}}}]
    (closeness-all g)
    =>
    {1 {:neigh #{0 4 3}, :close 3.5}
     0 {:neigh #{1 3}, :close 3.0}
     3 {:neigh #{0 1 2}, :close 3.5}
     4 {:neigh #{1}, :close 2.333333343267441}
     2 {:neigh #{3}, :close 2.333333343267441}})) 
