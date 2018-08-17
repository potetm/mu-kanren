(ns mu-kanren.monad-clojure
  "A more data-centric, yet still monadic approach.

  This is similar to what tbaldridge did in
  https://github.com/halgari/clojure-tutorial-source/blob/master/src/logic_tutorials/episode1.clj"
  (:refer-clojure :exclude [== conj disj]))

(defn lvar
  ([]
   (lvar ""))
  ([n]
   (gensym (str n "_"))))

(def lvar? symbol?)

(defn walk [s v]
  (if-some [pr (get s v)]
    (if (lvar? pr)
      (walk s pr)
      pr)
    v))

(defn unify [s u v]
  (let [u (walk s u)
        v (walk s v)]
    (cond
      (and (lvar? u)
           (lvar? v)
           (= u v)) s
      (lvar? u) (assoc s u v)
      (lvar? v) (assoc s v u)
      (= u v) s)))

(defn == [a b]
  (fn [s]
    (if-some [s' (unify s a b)]
      [s']
      [])))

(defn conj [& goals]
  (fn [s]
    (reduce (fn [s' g]
              (mapcat g s'))
            [s]
            goals)))

(defn disj [& goals]
  (fn [s]
    (mapcat (fn [g]
              (g s))
            goals)))

(defn conde [& goals]
  (apply disj (map (partial apply conj)
                   goals)))

(defmacro fresh [lvars & goals]
  `(let [~@(mapcat (fn [v]
                     `[~v (lvar ~(name v))])
                   lvars)]
     (conj ~@goals)))

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
         res# ((conj ~@goals) {})]
     (reify-lvars res# lvars#)))

(comment
  (run [a b c]
       (conde
         [(== a 1)
          (== a c)]
         [(== c 10)
          (== a 20)]
         [(== a 1)
          (== a 2)])))
