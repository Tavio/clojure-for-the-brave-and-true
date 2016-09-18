(ns chapter5.core
  (:gen-class))

(defn my-comp
  ([f] f)
  ([f & other-functions]
   (fn [& args]
     (f (apply (apply my-comp other-functions) args)))))

(defn natural-numbers
  ([] (natural-numbers -1))
  ([n]
   (let [next (inc n)]
     (cons next (lazy-seq (natural-numbers next))))))

(defn fibonacci-numbers
  ([]
   (cons 0 (lazy-seq (fibonacci-numbers 0))))
  ([last]
   (cons 1 (lazy-seq (fibonacci-numbers 0 1))))
  ([second-last last]
   (let [new-second-last last
         new-last (+ second-last last)]
     (cons new-last (lazy-seq (fibonacci-numbers new-second-last new-last))))))


(def fib-seq 
  ((fn rfib [a b] 
     (lazy-seq (cons a (rfib b (+ a b)))))
   0 1))

((fn my-assoc-in
    [m [k & ks] val]
    (if-not (coll? m)
      val
      (assoc m k (my-assoc-in (get m k) ks val))))
 {:a 1 :b [1 {:c 4 :d 5} 3]} [:b 1 :d] 6)
