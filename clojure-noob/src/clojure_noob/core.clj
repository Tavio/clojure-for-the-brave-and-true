(ns clojure-noob.core
  (:gen-class))

(def asym-hobbit-body-parts [{:name "head" :size 3}
                             {:name "left-eye" :size 1}
                             {:name "left-ear" :size 1}
                             {:name "mouth" :size 1}
                             {:name "nose" :size 1}
                             {:name "neck" :size 2}
                             {:name "left-shoulder" :size 3}
                             {:name "left-upper-arm" :size 3}
                             {:name "chest" :size 10}
                             {:name "back" :size 10}
                             {:name "left-forearm" :size 3}
                             {:name "abdomen" :size 6}
                             {:name "left-kidney" :size 1}
                             {:name "left-hand" :size 2}
                             {:name "left-knee" :size 2}
                             {:name "left-thigh" :size 4}
                             {:name "left-lower-leg" :size 3}
                             {:name "left-achilles" :size 1}
                             {:name "left-foot" :size 2}])
(defn matching-part
  [part]
  {:name (clojure.string/replace (:name part) #"^left-" "right-")
   :size (:size part)})

(defn matching-alien-parts
  [part]
  (let [radial-part-match (re-matches #"^left-(.*)" (:name part))]
    (if (nil? radial-part-match)
      [part]
      (let [radial-part (get radial-part-match 1)] 
           [{:name (str radial-part "-1") :size (:size part)}
            {:name (str radial-part "-2") :size (:size part)}
            {:name (str radial-part "-3") :size (:size part)}
            {:name (str radial-part "-4") :size (:size part)}
            {:name (str radial-part "-5") :size (:size part)}
            ]))))

(defn matching-n-parts
  [part num_parts]
  (let [radial-part-match (re-matches #"^left-(.*)" (:name part))]
    (if (nil? radial-part-match)
      [part]
      (let [radial-part (get radial-part-match 1)] 
        (loop [n 1
               matching_parts []]
          (if (> n num_parts)
            matching_parts
            (recur (inc n) (conj matching_parts {:name (str radial-part "-" n) :size (:size part)}))))))))

(defn symmetrize-body-parts
  [asym-body-parts]
  (loop [remaining-asym-parts asym-body-parts
         final-body-parts []]
    (if (empty? remaining-asym-parts)
      final-body-parts
      (let [[part & remaining] remaining-asym-parts]
        (recur remaining
               (into final-body-parts
                     (set [part (matching-part part)])))))))

(defn symmetrize-alien-body-parts
  [asym-body-parts]
  (loop [remaining-asym-parts asym-body-parts
         final-body-parts []]
    (if (empty? remaining-asym-parts)
      final-body-parts
      (let [[part & remaining] remaining-asym-parts]
        (recur remaining
               (into final-body-parts
                     (matching-alien-parts part)))))))

(defn symmetrize-n-body-parts
  [asym-body-parts num-symmetric]
  (loop [remaining-asym-parts asym-body-parts
         final-body-parts []]
    (if (empty? remaining-asym-parts)
      final-body-parts
      (let [[part & remaining] remaining-asym-parts]
        (recur remaining
               (into final-body-parts
                     (matching-n-parts part num-symmetric)))))))

(defn symmetrize-body-parts
  [asym-body-parts]
  (loop [remaining-asym-parts asym-body-parts
         final-body-parts []]
    (if (empty? remaining-asym-parts)
      final-body-parts
      (let [[part & remaining] remaining-asym-parts]
        (recur remaining
               (into final-body-parts
                     (set [part (matching-part part)])))))))



(defn better-symmetrize-body-parts
  [asym-body-parts]
  (reduce (fn [full-body-parts asym-body-part]
            (into full-body-parts
                  (set [asym-body-part (matching-part asym-body-part)])))
          []
          asym-body-parts))


(defn hit
  [asym-body-parts symmetrize-f]

  (let [sym-parts (symmetrize-f asym-body-parts)
        body-part-size-sum (reduce + (map :size sym-parts))
        target (rand body-part-size-sum)]
    (loop [[part & remaining] sym-parts
           accumulated-size (:size part)]
      (if (> accumulated-size target)
        part
        (recur remaining (+ accumulated-size (:size (first remaining))))))))

(defn add-100
  [num]
  (+ 100 num))

(defn dec-maker
  [dec-by]
  #(- % dec-by))

(defn mapset
  [f coll]
  (set (map f coll)))

(def human-consumption [8.1 7.3 6.6 5.0])
(def critter-consumption [0.0 0.2 0.3 1.1])
(defn unify-diet-data
  [human critter]
  (map (fn [human-value critter-value]
         (hash-map :human human-value :critter critter-value))
       human critter))

(def sum #(reduce + %))

(defn stats
  [numbers]
  (map #(% numbers) [sum count]))

(defn map-inc
  [input-map]
  (reduce (fn [new-map [key val]]
            (assoc new-map key (inc val)))
          {}
          input-map))

(defn filter-less-than-four
  [input-map]
  (reduce (fn [new-map [key val]]
            (if (> val 4)
              (assoc new-map key (inc val))
              new-map))
          {}
          input-map))

((defn map-using-reduce
    [f coll]
    (apply list (reduce (fn [applied next-to-apply]
                           (conj applied (f next-to-apply)))
                         []
                         coll))) inc '(1 2 3))

((defn filter-using-reduce
    [predicate coll]
    (apply list  (reduce (fn [filtered next-to-filter]
                           (if (predicate next-to-filter)
                             (conj filtered next-to-filter)
                             filtered))
                         []
                         coll))) #(> % 4) '(11 12 13 1 2 3 4 5 6 7 8))

((defn some-using-reduce
    [predicate coll]
    (reduce (fn [has-some some-candidate]
              (or has-some (predicate some-candidate)))
            false
            coll)) #(and (> % 4) %)  [1 2 8 3 4 5])

(def vampire-database
  {0 {:makes-blood-puns? false, :has-pulse? true :name "McFishwich"}
   1 {:makes-blood-puns? false, :has-pulse? true :name "McMackson"}
   2 {:makes-blood-puns? true, :has-pulse? false :name "Damon Salvatore"}
   3 {:makes-blood-puns? true, :has-pulse? true :name "Mickey Mouse"}})

(defn vampire-related-details
  [social-security-number]
  (Thread/sleep 1000)
  (get vampire-database social-security-number))

(defn vampire?
  [person]
  (and (:makes-blood-puns? person) (not (:has-pulse? person)) person))

(defn identify-vampire
  [social-security-numbers]
  (first (filter vampire? (map vampire-related-details social-security-numbers))))

(defn even-numbers
  ([] (even-numbers 0))
  ([x] (cons x (lazy-seq (even-numbers (+ x 2))))) 
)

(defn my-conj
  [target & additions]
  (into target additions))

(defn my-into
  [to from]
  (apply conj to from))

(defn my-partial
  [f & partial-args]
  (fn [& additional-args]
    (apply f (into partial-args additional-args))))
  
(defn lousy-logger
  [log-level message]
  (condp = log-level
    :warn (clojure.string/lower-case message)
    :error (clojure.string/upper-case message)))

(def warn (partial lousy-logger :warn))
(def error (partial lousy-logger :error))

(defn my-complement
  [f]
  (fn [& args]
    (not (apply f args))))
