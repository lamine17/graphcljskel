(ns graphclj.tools
  (:require [clojure.string :as str])
  (:use midje.sweet))

(defn readfile [f]
    "Returns a sequence from a file f"
    (with-open [rdr (clojure.java.io/reader f)]
            (doall (line-seq rdr))))

;;  La fonction sortk trie une sequence selon certaines valeurs et renvoie la cle de chacun
;;  de ces valeurs dans l'ordre.

(defn sortk [e] (keys (into {} (sort-by second e))))

(fact "La fonction sortk fonctionne correctement."      
  (sortk {:a 5 :b 3 :v 7}) => '(:b :a :v)
  (sortk {:a 55 :b 22 :v 7}) => '(:v :b :a))
  
;;La fonction km classe un map donné en entré g selon l'ordre de
;;ces clé donné en parametre par la sequence k.

(defn km [g k]
  (loop[nm {}
        k k]
    (if(seq k)
      (recur (assoc nm (first k) (get g (first k))) (rest k))
      nm)))

(fact "La fonction km fonctionne correctement."      
  (let[g {1 {:neigh #{0 4 3}}
          0 {:neigh #{1 3}}
          3 {:neigh #{0 1 2}}
          4 {:neigh #{1}}
          2 {:neigh #{3}}}]
    (keys (km g '(0 1 2 3 4)))
    =>
    '(0 1 2 3 4))
  (let[g {1 {:neigh #{0 4 3}}
          0 {:neigh #{1 3}}
          3 {:neigh #{0 1 2}}
          4 {:neigh #{1}}
          2 {:neigh #{3}}}]
    (keys (km g '(2 3 4 0 1)))
    =>
    '(2 3 4 0 1)))
        

;;La fonction creatm crée une map a partir d'une autre map. 
;;La map retourné prend comme clés chacunes des clé de la map d'origine
;;et comme valeurs la valeurs pour chacune des clé de la map d'origine
;;leurs map en valeurs quand a la clé dans cette sous map elle
;;est transmis par l'argument l.

(defn creatm [g l]
  (loop[m {}
        g g]
    (if(seq g)
      (recur (assoc m (first (first g)) (get (second (first g)) l)) (rest g))
      m)))

(fact "La fonction creatm fonctionne correctement."      
  (let[g {1 {:neigh #{0 4 3}}
          0 {:neigh #{1 3}}
          3 {:neigh #{0 1 2}}
          4 {:neigh #{1}}
          2 {:neigh #{3}}}]
    (creatm g :neigh)
    =>
    {1 #{0 4 3} 0 #{1 3} 3 #{0 1 2} 4 #{1} 2 #{3}})
  (let[g {1 {:neigh #{0 4 3} :es 1}
          0 {:neigh #{1 3} :es 2}
          3 {:neigh #{0 1 2} :es 3}
          4 {:neigh #{1} :es 4}
          2 {:neigh #{3} :es 5}}]
      (creatm g :es)
    =>
    '{1 1 0 2 3 3 4 4 2 5}))

;;La fonction rank-nodes trie un graph selon un criteere donné en entrée.

(defn rank-nodes [g l]
  (km g (sortk (creatm g l))))


(fact "La fonction rank-nodes fonctionne correctement."      
  (let[g {1 {:neigh #{0 4 3} :es 7}
          0 {:neigh #{1 3} :es 5} 
          3 {:neigh #{0 1 2} :es 8}
          4 {:neigh #{1} :es 3}
          2 {:neigh #{3} :es 1}}]
    (keys (rank-nodes g :es))
    =>
    '(2 4 0 1 3)))
          
  
  

(defn generate-colors [n]
    (let [step 10]
     (loop [colors {}, current [255.0 160.0 122.0], c 0]
       (if (= c (inc n))
         colors
         (recur (assoc colors c (map #(/ (mod (+ step %) 255) 255) current))
                (map #(mod (+ step %) 255) current) (inc c))))))
 
;;La fonction a1 crée une chaine de caractere a partir d'une sequence, dans cette chaine
;; de caractere chaque element est separée par un espace.

(defn a1 [sc n]
  (str "" (reduce str (interpose " " (get sc (first (first n)))))))

(fact "La fonction rank-nodes fonctionne correctement."      
    (a1 {1 (list 25 15 22)} (list (list 1)))
    =>
    "25 15 22")

;;addlink a pour utilité d'ajouter a une sequence s
;;chaque lien d'un certain graph g.
;;Pour eviter les doublons les
;;liens sont construits dans l'ordre decroissant.

(defn addlinkin [g n s]
  (loop[l (get (get g n) :neigh)
        r s]
    (if(seq l)
      (if(> n (first l))
        (recur (rest l) (cons [n (first l)] r))
        (recur (rest l) (cons [(first l) n] r)))
      r)))

(fact "La fonction addlinkin fonctionne correctement."      
  (let[g {1 {:neigh #{0 4 3} :es 7}
          0 {:neigh #{1 3} :es 5} 
          3 {:neigh #{0 1 2} :es 8}
          4 {:neigh #{1} :es 3}
          2 {:neigh #{3} :es 1}}]
    (addlinkin g 1 #{})
    =>
    '([3 1] [4 1] [1 0]))
  (let[g {1 {:neigh #{0 4 3} :es 7}
          0 {:neigh #{1 3} :es 5} 
          3 {:neigh #{0 1 2} :es 8}
          4 {:neigh #{1} :es 3}
          2 {:neigh #{3} :es 1}}]
    (addlinkin g 4 #{})
    =>
    '([4 1])))
        
;;La fonction addlinks renvoie sous la forme d'un ensemble la totalité 
;;des liens d'un graph sans le moindre doublons.
  
(defn addlinks [g]
  (loop[m #{}
        ng g]
    (if(seq ng)
      (recur (addlinkin g (first (first ng)) m) (rest ng))
      (set m))))

(fact "La fonction addlinks fonctionne correctement."      
  (let[g {1 {:neigh #{0 4 3} :es 7}
          0 {:neigh #{1 3} :es 5} 
          3 {:neigh #{0 1 2} :es 8}
          4 {:neigh #{1} :es 3}
          2 {:neigh #{3} :es 1}}]
    (addlinks g)
    =>
    #{[3 0] [3 2] [3 1] [4 1] [1 0]})
  (let[g {1 {:neigh #{0 4 3} :es 7}
          0 {:neigh #{1} :es 5} 
          3 {:neigh #{ 1 2} :es 8}
          4 {:neigh #{1} :es 3}
          2 {:neigh #{3} :es 1}}]
    (addlinks g)
    =>
    #{ [3 2] [3 1] [4 1] [1 0]}))

;;La fonction to-dot renvoie une map sous un format .dot

(defn to-dot [g]
  (let[cs (generate-colors (dec (count g)))]
    (loop[ng g
          m {}]
      (if(seq ng)
        (recur (rest ng) (assoc m (first (first ng)) {:style "filled" :color (a1 cs ng)}))
        (assoc m :links (addlinks g))))))
  
(fact "La fonction to-dot fonctionne correctement."      
  (let[g {1 {:neigh #{0 4 3} :es 7}
          0 {:neigh #{1 3} :es 5} 
          3 {:neigh #{0 1 2} :es 8}
          4 {:neigh #{1} :es 3}
          2 {:neigh #{3} :es 1}}]
    (to-dot g)
    =>
    {1 {:style "filled", :color "0.0784313725490196 0.7058823529411765 0.5568627450980392"} 
      0 {:style "filled", :color "0.0392156862745098 0.6666666666666666 0.5176470588235295"} 
      3 {:style "filled", :color "0.1568627450980392 0.7843137254901961 0.6352941176470588"}
      4 {:style "filled", :color "0.19607843137254902 0.8235294117647058 0.6745098039215687"} 
      2 {:style "filled", :color "0.11764705882352941 0.7450980392156863 0.596078431372549"}
      :links #{[1 0] [3 0] [4 1] [3 1] [3 2]}}))

  
