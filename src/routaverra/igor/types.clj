(ns routaverra.igor.types
  (:require [clojure.string]))

(def Numeric ::numeric)
(def Set ::set)
(def Bool ::boolean)
(def Keyword ::keyword)

(def all-decision-types
  #{Numeric
    Set
    Bool
    Keyword})

(defn domain->type [domain]
  (let [types (keys domain)]
    (if (= 1 (count types))
      (first types)
      (throw (ex-info
              (str "Ambiguous type: decision is used in both "
                   (clojure.string/join " and " (sort types))
                   " contexts. Use domain or universe to disambiguate.")
              {:types (set types)})))))
