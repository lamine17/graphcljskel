(ns graphclj.graph
  (:require [clojure.string :as str])
  (:use midje.sweet))

;; Generate a graph from the lines

(use ['clojure.string :only '(split)])

;; La fonction str-to-ints convertit 
;; une chaine de caractere en entrée 
;; en une sequence de nombre.
;; Si l'argument en entrée n'est pas une chaine de caractere
;; la fonction le renvoie tel quel.

(defn str-to-ints
  [string]
  (if(string? string)
    (map #(Integer/parseInt %)
        (split string #" "))
    string))

(fact "La fonction str-to-ints fonctionne correctement."      
    (str-to-ints "25 23")
    =>
    '(25 23)
    (str-to-ints 25)
    =>
    25)
  
;;La fonction nir prend en argument une map puis renvoie une map ou chaque
;;clé de la map en entrée a pour valeur une sous-map dans la quelle sa valeur
;;d'origine a pour clé :neigh. Chacune des valeurs doit etre un ensemble

(defn nir [m]
  (loop[mo m
        mr {}]
    (if(seq mo)
      (recur (rest mo) (assoc mr (first (first mo)) (array-map :neigh (set (second (first mo))))))
      mr)))
  
(fact "La fonction nir fonctionne correctement."      
    (nir {:a #{1} :b #{2} :c #{3}})
    =>
    {:a {:neigh #{1}} :b {:neigh #{2}} :c {:neigh #{3}}})

;;Les fonctions ajoutd, ajoutg
;;et gen-graph permet de creer
;;depuis une sequence de liens 
;;un graph.
    
(defn ajoutd [l m]
  (if(contains? m (second (str-to-ints l)))
    (assoc m (second (str-to-ints l)) (cons (first (str-to-ints l)) (get m (second (str-to-ints l)))))
    (assoc m (second (str-to-ints l)) (list (first (str-to-ints l))))))

(defn ajoutg [l m]
  (if(contains? m (first (str-to-ints l)))
    (ajoutd l (assoc m (first (str-to-ints l)) (cons (second (str-to-ints l)) (get m (first (str-to-ints l))))))
    (ajoutd l (assoc m (first (str-to-ints l)) (list (second (str-to-ints l)))))))

(defn gen-graph [lines]
    (loop[l lines
          m {}]
      (if(seq l)
        (recur (rest l) (ajoutg (first l) m))
        (nir m))))

(fact "La fonction gen-graph fonctionne correctement."      
    (gen-graph (list [0 1] [1 2]))
    =>
    {0 {:neigh #{1}} 
     1 {:neigh #{0,2}} 
     2 {:neigh #{1}}})  

;;La fonction plinks prends une sequence 
;;en argument, puis sur les elements de cette
;;sequence elle prend avec une probabilité
;;p chacun des elements de cette sequence
;;pour les incorporer dans la sequence de retour.


(defn plinks [ls p]
  (loop[lo ls
        lr (list)]
    (if(seq lo)
      (if(> p (rand))
        (recur (rest lo) (cons (first lo) lr))
        (recur (rest lo) lr))
      lr)))

(fact "La fonction plinks fonctionne correctement."      
    (let [s #{1 2 3 4 5 6}]
      (let [r (plinks s 0.5)]
        r => (clojure.set/intersection r s)))

    (let [s #{:a :b :c :g}]
      (let [r (plinks s 0.5)]
        r => (clojure.set/intersection r s))))  

;;La fonction glinks1 prend en argument une liste et un element
;;elle renvoie un liste des intersections entre cet element et 
;;les elements de cette liste, sauf l'intersection de l'element passé en argument 
;;et la valeur dans la liste qui lui est egal.

(defn glinks1 [l e]
  (loop[l1 (list)
        l2 l]
    (if(seq l2)
      (if (not= e (first l2))
        (if(> e (first l2))
          (recur (cons (vector e (first l2)) l1) (rest l2))
          (recur (cons (vector (first l2) e) l1) (rest l2)))
        (recur l1 (rest l2)))
      l1)))

(fact "La fonction glinks1 fonctionne correctement."      
    (glinks1 (list 1 2 3) 1) => '([3 1] [2 1])

    (glinks1 (list 5 9 8) 1) => '([8 1] [9 1] [5 1]))  


;;La fonction glinks renvoie un ensemble de liens 
;;possible entre des noeuds allant de 0 à n.

(defn glinks [n]
  (let [l (range 0 n)]
    (loop[l1 (range 0 n)
          lr (list)]
      (if(seq l1)
        (recur (rest l1) (concat lr (glinks1 l (first l1))))
        (set lr)))))

(fact "La fonction glinks fonctionne correctement."      
    (glinks 1) => #{}

    (glinks 3) => #{[1 0] [2 0] [2 1]}) 
        
;;La fonction erdos-renyi-rnd renvoie un graph
;;aleatoire contantant n noeuds dont chaque noeuds
;;forme deux a deux un liens selon une probabilité p.

(defn erdos-renyi-rnd [n p]
  (gen-graph ( plinks (glinks n) p)))

(fact "La fonction erdos-renyi-rnd semble fonctionner correctement."      
    (erdos-renyi-rnd 100 0) => {}

    (erdos-renyi-rnd 3 1) => {2 {:neigh #{0 1}}, 1 {:neigh #{0 2}}, 0 {:neigh #{1 2}}}) 
