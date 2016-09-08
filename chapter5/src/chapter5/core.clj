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

