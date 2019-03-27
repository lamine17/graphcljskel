(ns graphclj.tools
  (:require [clojure.string :as str]))

(defn readfile [f]
    "Returns a sequence from a file f"
    (with-open [rdr (clojure.java.io/reader f)]
            (doall (line-seq rdr))))

(defn sortk [e] (keys (into {} (sort-by second e))))

(defn km [g k]
  (loop[nm {}
        k k]
    (if(seq k)
      (recur (assoc nm (first k) (get g (first k))) (rest k))
      nm)))

(defn creatm [g l]
  (loop[m {}
        g g]
    (if(seq g)
      (recur (assoc m (first (first g)) (get (second (first g)) l)) (rest g))
      m)))

(defn rank-nodes [g l]
  (km g (sortk (creatm g l))))
  
  
  

(defn generate-colors [n]
    (let [step 10]
     (loop [colors {}, current [255.0 160.0 122.0], c 0]
       (if (= c (inc n))
         colors
         (recur (assoc colors c (map #(/ (mod (+ step %) 255) 255) current))
                (map #(mod (+ step %) 255) current) (inc c))))))
      

(defn a1 [sc n]
  (str "" (reduce str (interpose " " (get sc (first (first n)))))))

(defn addlinkin [g n s]
  (loop[l (get (get g n) :neigh)
        r s]
    (if(seq l)
      (if(> n (first l))
        (recur (rest l) (cons [n (first l)] r))
        (recur (rest l) (cons [(first l) n] r)))
      r)))
        

  
(defn addlinks [g]
  (loop[m #{}
        ng g]
    (if(seq ng)
      (recur (addlinkin g (first (first ng)) m) (rest ng))
      (set m))))

(defn to-dot [g]
  (let[cs (generate-colors (dec (count g)))]
    (loop[ng g
          m {}]
      (if(seq ng)
        (recur (rest ng) (assoc m (first (first ng)) {:style "filled" :color (a1 cs ng)}))
        (assoc m :links (addlinks g))))))
  
  
