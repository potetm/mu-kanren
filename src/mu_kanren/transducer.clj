(ns mu-kanren.transducer
  "Use transducers to implement operations on streams of data."
  (:require [clojure.java.io :as jio])
  (:import
    (clojure.lang ISeq))
  (:refer-clojure :exclude [conj disj ==]))

(defn lvar
  ([]
   (lvar ""))
  ([n]
   (gensym (str n "_"))))

(def lvar? symbol?)

(defn walk [s v]
  (if-some [[_k pr] (find s v)]
    (if (lvar? pr)
      (recur s pr)
      pr)
    v))

(defprotocol ILCons
  (lfirst [this])
  (lnext [this]))

(defn lcons? [x]
  (satisfies? ILCons x))

(extend-type ISeq
  ILCons
  (lfirst [this]
    (first this))
  (lnext [this]
    (next this)))

(defrecord LCons [h t]
  ILCons
  (lfirst [this]
    h)
  (lnext [this]
    t))

(defn lcons [h t]
  (->LCons h t))

(defn unify [s u v]
  (let [u (walk s u)
        v (walk s v)]
    (cond
      (and (lvar? u)
           (lvar? v)
           (= u v))
      s

      (lvar? u)
      (assoc s u v)

      (lvar? v)
      (assoc s v u)

      (and (lcons? u)
           (lcons? v))
      (some-> s
              (unify (lfirst u)
                     (lfirst v))
              (recur (lnext u)
                     (lnext v)))

      (= u v) s)))

(defn == [u v]
  (keep (fn [s]
          (unify s u v))))

(def conj comp)

(defn disj [& goals]
  (mapcat (fn [s]
            (eduction (mapcat (fn [g]
                                (eduction g [s])))
                      goals))))

(defn conde [& goals]
  (apply disj (map (partial apply conj)
                   goals)))

(defmacro fresh [lvars & body]
  `(let [~@(mapcat (fn [v]
                     `[~v (lvar ~(name v))])
                   lvars)]
     (conj ~@body)))

(defn conso [h t o]
  (== (lcons h t) o))

(defn firsto [h t]
  (fresh [r]
    (conso h r t)))

(defn resto [r t]
  (fresh [h]
    (conso h r t)))

(defmacro defer [g]
  `(mapcat (fn [s#]
             (eduction ~g
                       [s#]))))

(defn membero [v col]
  (conde
    [(firsto v col)]
    [(fresh [t]
       (resto t col)
       (defer (membero v t)))]))

(defn reify-lvars [lvars]
  (map (fn [s]
         (mapv (partial walk s)
               lvars))))

(defmacro run [lvars & goals]
  `(let [lvars# ~(mapv (fn [v]
                         `(lvar ~(name v)))
                       lvars)
         ~lvars lvars#]
     (eduction (comp (conj ~@goals)
                     (reify-lvars lvars#))
               [{}])))


(comment
  (sequence (== 1 1)
            [{}])

  (sequence (disj (== 'a 1)
                  (== 'b 2)
                  (conj (== 'a 5)
                        (== 'b 20)))
            [{}])

  (sequence (conde
              [(== 'a 10)
               (== 'a 'b)]
              [(== 'a 20)
               (== 'a 15)]
              [(== 'b 35)
               (== 'b 'c)])
            [{}])

  (run [a b]
       (conde [(== 'a 10)
               (== 'a 'b)]
              [(== 'a 20)
               (== 'a 15)]
              [(== 'b 35)
               (== 'b 'c)]))

  (run [q]
       (conso 1 '(2 3 4) q))
  (sequence (comp (conj (membero 'q (range 100)))
                  (reify-lvars ['q]))
            [{}])
  (macroexpand '(run [q]
                     (membero q (range 10))))
  (run [q]
       (membero q (range 100))
       (membero q (range 97 120)))


  (require '[clojure.data.xml :as xml])
  (def xml-data (xml/parse-str (slurp "resources/eve_data.xml")))

  )
