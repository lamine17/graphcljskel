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

;;La fonction ajoutd ajoute a un noeud d'une map
;;m un lien selon si il est deja present dans la liste de se lien
;;selon l'element droit d'un lien l
    
(defn ajoutd [l m]
  (if(contains? m (second (str-to-ints l)))
    (assoc m (second (str-to-ints l)) (cons (first (str-to-ints l)) (get m (second (str-to-ints l)))))
    (assoc m (second (str-to-ints l)) (list (first (str-to-ints l))))))

(fact "La fonction nir fonctionne correctement."      
    (nir {:a #{1} :b #{2} :c #{3}})
    =>
    {:a {:neigh #{1}} :b {:neigh #{2}} :c {:neigh #{3}}})

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
      
      
(defn plinks [ls p]
  (loop[lo ls
        lr (list)]
    (if(seq lo)
      (if(> p (rand))
        (recur (rest lo) (cons (first lo) lr))
        (recur (rest lo) lr))
      lr)))

(defn glinks1 [l e]
  (loop[l1 (list)
        l2 l]
    (if(seq l2)
      (if (not= e (first l2))
        (recur (cons (vector (first l2) e) l1) (rest l2))
        (recur l1 (rest l2)))
      l1)))




(defn glinks [n]
  (let [l (range 0 n)]
    (loop[l1 (range 0 n)
          lr (list)]
      (if(seq l1)
        (recur (rest l1) (concat lr (glinks1 l (first l1))))
        lr))))
        

(defn erdos-renyi-rnd [n p]
  (gen-graph ( plinks (glinks n) p)))

