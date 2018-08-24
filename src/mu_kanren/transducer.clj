(ns mu-kanren.transducer
  "Use transducers to implement operations on streams of data."
  (:require
    [clojure.math.combinatorics :as combo]
    [clojure.set :as set]
    [clojure.walk :as walk])
  (:import
    (clojure.lang ISeq))
  (:refer-clojure :exclude [==]))

(defn lvar
  ([]
   (lvar ""))
  ([n]
   (gensym (str n "_"))))

(def lvar? symbol?)

(def grounded? (complement symbol?))

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

(def conj* comp)

;; from medley
(defn interleave-all
  ([& colls]
   (lazy-seq
     (let [ss (keep seq
                    colls)]
       (if (seq ss)
         (concat (map first
                      ss)
                 (apply interleave-all
                        (map rest
                             ss))))))))

(defn intermap [f coll]
  (apply interleave-all (map f coll)))

;; breadth first
(defn disj* [& goals]
  (mapcat (fn [s]
            (intermap (fn [g]
                        (sequence g [s]))
                      goals))))

(comment
  ;; depth first
  (defn disj* [& goals]
    (mapcat (fn [s]
              (mapcat (fn [g]
                        (sequence g [s]))
                      goals)))))

(defn conde [& goals]
  (apply disj* (map (partial apply conj*)
                    goals)))

(defmacro fresh [lvars & body]
  `(let [~@(mapcat (fn [v]
                     `[~v (lvar ~(name v))])
                   lvars)]
     (conj* ~@body)))

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
     (eduction (comp (conj* ~@goals)
                     (reify-lvars lvars#))
               [{}])))

(defn keys-for-coll [coll]
  (cond
    (nil? coll) nil
    (vector? coll) (range (count coll))
    (map? coll) (keys coll)
    :else (throw (ex-info "No keys for coll"
                          {:coll coll}))))

(defn to-db-seq
  ([struct]
   (to-db-seq struct []))
  ([struct path]
   (vec (mapcat (fn [k]
                  (let [v (get struct k)]
                    (if (associative? v)
                      (let [p' (conj path k)]
                        (cons [path k p']
                              (to-db-seq v p')))
                      [[path k v]])))
                (keys-for-coll struct)))))

(defn relation
  ([data e k v]
   (mapcat (fn [s]
             (let [e' (walk s e)
                   k' (walk s k)
                   v' (walk s v)]
               (keep (fn [x]
                       (unify s
                              (list* x)
                              (list e' k' v')))
                     data)))))
  ([data {:keys [ekv kve vek]} e k v]
   (mapcat (fn [s]
             (condp = [(grounded? e)
                       (grounded? k)
                       (grounded? v)]
               [false false false] (eduction (relation data e k v)
                                             [s])

               [true true true] (when (get-in ekv [e k v])
                                  [s])

               [true false false]
               (eduction (mapcat (fn [[k' vs']]
                                   (eduction (keep (fn [v']
                                                     (unify s
                                                            (list k' v')
                                                            (list k v))))
                                             vs')))
                         (get ekv e))

               [true true false]
               (eduction (keep (fn [v']
                                 (unify s v v')))
                         (get-in ekv [e k]))

               [true false true]
               (eduction (keep (fn [k']
                                 (unify s k k')))
                         (get-in vek [v e]))

               [false true false]
               (eduction (mapcat (fn [[v' es']]
                                   (eduction (keep (fn [e']
                                                     (unify s
                                                            (list v' e')
                                                            (list v e))))
                                             es')))
                         (get kve k))

               [false true true]
               (eduction (keep (fn [e']
                                 (unify s e e')))
                         (get-in kve [k v]))

               [false false true]
               (eduction (mapcat (fn [[e' ks']]
                                   (eduction (keep (fn [k']
                                                     (unify s
                                                            (list e' k')
                                                            (list e k))))
                                             ks')))
                         (get vek v)))))))

(defn merge-unifiers [[acc other & others]]
  (if other
    (let [ks (set/intersection (set (keys acc))
                               (set (keys other)))]
      (when (or (empty? ks)
                (= (select-keys acc ks)
                   (select-keys other ks)))
        (recur (cons (merge acc other)
                     others))))
    acc))

(defn conj-set [& goals]
  (mapcat (fn [s]
            (let [unifiers (map (fn [g]
                                  (sequence g [s]))
                                goals)]
              (keep merge-unifiers
                    (apply combo/cartesian-product
                           unifiers))))))

(defn index [data]
  (let [set-conj (fnil conj #{})]
    {:ekv (reduce (fn [acc [e k v]]
                    (update-in acc
                               [e k]
                               set-conj
                               v))
                  {}
                  data)
     :kve (reduce (fn [acc [e k v]]
                    (update-in acc
                               [k v]
                               set-conj
                               e))
                  {}
                  data)
     :vek (reduce (fn [acc [e k v]]
                    (update-in acc
                               [v e]
                               set-conj
                               k))
                  {}
                  data)}))

(comment
  (sequence (== 1 1)
            [{}])

  (sequence (disj* (== 'a 1)
                   (== 'b 2)
                   (conj* (== 'a 5)
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
  (sequence (comp (conj* (membero 'q (range 100)))
                  (reify-lvars ['q]))
            [{}])
  (macroexpand '(run [q]
                     (membero q (range 10))))
  (run [q]
       (membero q (range 100))
       (membero q (range 97 120)))


  (require '[clojure.data.xml :as xml])
  (def xml-data (walk/postwalk (fn [x]
                                 (if (seq? x)
                                   (vec x)
                                   x))
                               (xml/parse-str (slurp "resources/eve_data.xml"))))

  (def $ (to-db-seq xml-data))
  (run [p v]
       (fresh [p' a]
              (relation $ p :tag :min)
              (relation $ p :content p')
              (relation $ p' a v)))

  (run [q]
       (conde
         [(conde
            [(== q 1)]
            [(== q 11)]
            [(== q 111)]
            [(conde
               [(== q 2)]
               [(== q 22)])])]
         [(conde
            [(== q 3)]
            [(== q 33)])]
         [(== q 44)]))


  (def data
    [[:fernand :kills :fernand]
     [:fernand :kills :ali]
     [:fernand :father :albert]
     [:mercedes :mother :albert]
     [:albert :duels :edmond]
     [:mercedes :husband :fernand]
     [:fernand :wife :mercedes]
     [:mercedes :cousin :fernand]
     [:fernand :cousin :mercedes]
     [:louis :father :edmond]
     [:edmond :saves :pierre]
     [:pierre :employer :edmond]
     [:pierre :father :maximillien]
     [:pierre :father :julie]])

  (def idx (index data))

  (run [a b c]
       (conj*
         (relation data a :father b)
         (relation data c :cousin a)
         (relation data c :mother b)))

  (run [a b c]
       (conj-set
         (relation data a :father b)
         (relation data c :cousin a)
         (relation data c :mother b)))

  (run [a b c]
       (conj*
         (relation data idx a :father b)
         (relation data idx c :cousin a)
         (relation data idx c :mother b)))

  (dotimes [_ 1000]
    (time (run [v]
               (relation data :fernand :kills :fernand))))

  (dotimes [_ 1000]
    (time (run [k v]
               (relation data idx k v :fernand))))

  (dotimes [_ 1000]
    (time (run [a b c]
               (conj*
                 (relation data idx a :father b)
                 (relation data idx c :cousin a)
                 (relation data idx c :mother b)))))
  )
