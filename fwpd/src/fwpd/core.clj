(ns fwpd.core)
(def filename "suspects.csv")

(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer. str))

(def conversions {:name identity
                  :glitter-index str->int})

(def validations {:name (complement nil?)
                  :glitter-index (complement nil?)})

(defn convert
  [vamp-key value]
  ((get conversions vamp-key) value))

(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map #(clojure.string/split % #",")
       (clojure.string/split string #"\n")))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
                 {}
                 (map vector vamp-keys unmapped-row)))
       rows))

(defn glitter-filter
  [minimum-glitter records]
  (map :name
       (filter
        #(>= (:glitter-index %) minimum-glitter)
        records)))

(defn append 
  [suspects-list new-suspect]
  (conj suspects-list new-suspect ))

(defn validate
  [validation-map record]
  (reduce (fn [invalid-fields
               [validation-key validation-func]]
            (if ((validation-key validations) (validation-key record))
              invalid-fields
              (conj invalid-fields validation-key)))
          []
          validation-map))

(defn records-to-csv
  [records]
  (clojure.string/join
   "\n"   
   (map 
    (fn [record]
      (clojure.string/join
       ","
       (map
        (fn [[key val]]
          val)
        record)))
    records)))
