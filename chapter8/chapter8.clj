(ns chpter8)

(defmacro my-print
  [expression]
  (list 'let ['result expression]
        (list 'println 'result)
        'result))


(my-print "hahaha")

(defn criticize-code
  [criticism code]
  `(println ~criticism (quote ~code)))

(defmacro code-critic
  [bad good]
  `(do ~@(map #(apply criticize-code %)
             [["Bad code: " bad] ["Good code: " good]])))

(code-critic (1 + 1) (+ 1 1))

;; Potion shop exercises:

(def order-details-validations
  {:name
   ["Name may not be empty" not-empty]
   :email
   ["Email may not be empty" not-empty

    "Email does not look like an email"
    #(or(empty? %) (re-seq #"@" %))]})

(defn error-messages-for
  "Returns a seq of error messages"
  [to-validate validation-pairs]
  (->> (partition 2 validation-pairs)
       (filter #(not ((second %) to-validate)))
       (map first)))

(defn validate
  "Returns a map with a vector of errors for each key"
  [to-validate validations]
  (reduce (fn [errors validation]
            (let [[field-name validation-group] validation
                  field-value (field-name to-validate)
                  field-errors (error-messages-for field-value validation-group)]
              (if (not-empty field-errors)
                (assoc errors field-name field-errors)
                errors)))
          {}
          validations))

(defmacro if-valid
  "Handle validation more concisely"
  [to-validate validations errors-name & then-else]
  `(let [~errors-name (validate ~to-validate ~validations)]
     (if (empty? ~errors-name)
       ~@then-else)))

(defmacro when-valid
  [to-validate validations errors-name & then-else]
  `(let [~errors-name (validate ~to-validate ~validations)]
     (if (empty? ~errors-name)
      (do ~@then-else))))

(def order-details
  {:name "Mitchard Blimmons"
   :email "mitchard.blimmons@gmail.com"})

(if-valid order-details order-details-validations my-error-name
          (println :success)
          (println :failure my-error-name))

;; End-of-chapter exercises

(when-valid order-details order-details-validations my-error-name
            (println :success)
            (println "Also this."))

(defmacro or-as-macro
  ([] nil)
  ([x] x)
  ([x & next]
   `(let [or# ~x]
      (if or# or# (or-as-macro ~@next)))))

(or-as-macro false true nil)

(def character
  {:name "Smooches McCutes"
   :attributes {:intelligence 10
                :strength 4
                :dexterity 5}})

(defn create-attr-fn
  [fn-name attr]
  `(def ~fn-name (comp ~attr :attributes)))

(defmacro defattrs
  [& fn-attrs]
  `(do ~@(map #(apply create-attr-fn %) (partition 2 fn-attrs))))

(defattrs c-int :intelligence c-str :strength)

(c-int character)
(c-str character)

