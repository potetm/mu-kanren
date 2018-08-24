(ns mu-kanren.original
  "This is roughly similar to the original µKanren paper.

  The one notable exception is the fact that I did not include
  vars in the monadic value. This is ok because it's a toy implementation,
  and the var name gets prefixed in the resultant states."
  (:refer-clojure :exclude [==]))

(defn lvar
  ([]
   (lvar ""))
  ([n]
   (gensym (str n "_"))))

(defn lvar? [v]
  (symbol? v))

(defn walk [s u]
  (if-some [pr (get s u)]
    (if (lvar? pr)
      (recur s pr)
      pr)
    u))

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

(defn unit [s]
  [s])
(def mzero nil)

(defn mplus [& ss]
  (apply concat ss))

(defn bind [s g]
  (apply mplus (map g s)))

(defn == [a b]
  (fn [s]
    (if-some [s' (unify s a b)]
      (unit s')
      mzero)))

(defn conj*
  ([g] g)
  ([g1 g2]
   (fn [s]
     (bind (g1 s)
           g2)))
  ([a b & more]
   (conj* a (apply conj* b more))))

(defn disj* [& goals]
  (fn [s]
    (apply mplus
           (map (fn [g]
                  (g s))
                goals))))

(defn fresh*
  ([f]
   (fresh* nil f))
  ([n f]
   (fn [s]
     (let [v (lvar n)]
       ((f v) s)))))

(defn conde [& goals]
  (apply disj* (map (partial apply conj*)
                    goals)))

(defmacro fresh [vars & goals]
  (if (seq vars)
    `(fresh* (quote ~(first vars))
             (fn [~(first vars)]
               (fresh ~(rest vars)
                      ~@goals)))
    `(conj* ~@goals)))

(def empty-state {})
(defn call-empty-state [g]
  (g empty-state))

(defmacro run [n vars & goals]
  `(take ~n
         (call-empty-state (fresh ~vars
                                  ~@goals))))

(defmacro run* [vars & goals]
  `(call-empty-state (fresh ~vars
                            ~@goals)))

(comment

  ((fresh* (fn [q]
             (== q 5))) empty-state)
  (def a-and-b
    (conj* (fresh* (fn [a]
                     (== a 7)))
           (fresh* (fn [b]
                     (disj* (== b 5)
                            (== b 6))))))

  (a-and-b empty-state)

  ((conde
     [(fresh* (fn [a]
                (== a 1)))]
     [(fresh* (fn [a]
                (== a 2)))])
    empty-state)

  ((fresh [a b c]
          (conde
            [(== a 1)
             (== a b)
             (== c 3)]))
    {})

  (run*
    [a b c]
    (conde
      [(== a b)
       (== c b)])
    (conde
      [(== b 1)
       ]))
  )
