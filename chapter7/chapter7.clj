(ns chapter7
  (:refer-clojure))

(defmacro ignore-last-operand
  [function-call]
  (butlast function-call))

(defmacro infix
  [expr]
    (let [[term op [& rest-expr]] (partition-by (fn [elem]
                                              (and (not= elem +) (not= elem -)))
                                                expr)]
      (if (nil? op)
        (parse-term term)
      (list op (parse-term term) (parse-expr rest-expr)))))

(defn parse-term
  [term]
  (reduce (fn [parsed [op operand]]
            (list op parsed operand))
          (first term)
          (partition 2 (rest term))))

(infix (1 * 2 * 3))

(macroexpand '(infix (1 * 2 * 3)))
