(ns mu-kanren.monad-clojure
  "A more data-centric, yet still monadic approach.

  This is similar to what tbaldridge did in
  https://github.com/halgari/clojure-tutorial-source/blob/master/src/logic_tutorials/episode1.clj"
  (:refer-clojure :exclude [==])
  (:import
    (clojure.lang ISeq)))

(defn lvar
  ([]
   (lvar ""))
  ([n]
   (gensym (str n "_"))))

(def lvar? symbol?)

(def grounded? (complement lvar?))

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

(defn walk [s v]
  (if-some [[_ pr] (find s v)]
    (if (grounded? pr)
      pr
      (walk s pr))
    v))

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

(defn == [a b]
  (fn [s]
    (when-some [s' (unify s a b)]
      [s'])))

(defn -conj [& goals]
  (fn [s]
    (reduce (fn [s' g]
              (mapcat g s'))
            [s]
            goals)))

(defn -disj [& goals]
  (fn [s]
    (mapcat (fn [g]
              (g s))
            goals)))

(defn conde [& goals]
  (apply -disj (map (partial apply -conj)
                    goals)))

(defmacro fresh [lvars & goals]
  `(let [~@(mapcat (fn [v]
                     `[~v (lvar ~(name v))])
                   lvars)]
     (-conj ~@goals)))

(defn conso [h t o]
  (== (lcons h t) o))

(defn firsto [h t]
  (fresh [r]
         (conso h r t)))

(defn resto [r t]
  (fresh [h]
         (conso h r t)))

(defmacro defer [g]
  `(fn [s#]
     (~g s#)))

(defn membero [v col]
  (conde
    [(firsto v col)]
    [(fresh [t]
            (resto t col)
            (defer (membero v t)))]))

(defn relation [d e a v]
  (fn [s]
    (let [e' (walk s e)
          a' (walk s a)
          v' (walk s v)]
      (keep (fn [x]
              (unify s
                     (list* x)
                     (list e' a' v')))
            d))))

(defn reify-lvars [res lvars]
  (map (fn [r]
         (map (partial walk r)
              lvars))
       res))

(defmacro run [lvars & goals]
  `(let [~@(mapcat (fn [v]
                     `[~v (lvar ~(name v))])
                   lvars)
         lvars# ~lvars
         res# ((-conj ~@goals) {})]
     (reify-lvars res# lvars#)))

(comment
  (run [a b c]
       (conde
         [(== a 1)
          (== a c)]
         [(== c 10)
          (== a 20)]
         [(== a 1)
          (== a 2)]))

  (run [q]
       )

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

  (run [a b c]
       (relation data a :father b)
       (relation data c :cousin a)
       (relation data c :mother b)))
