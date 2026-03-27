(ns routaverra.igor.terms.introduced
  (:require [routaverra.igor.protocols :as protocols]
            [routaverra.igor.types :as types]
            [routaverra.igor.utils.string :refer [>>]]
            [routaverra.igor.api :as api]
            [routaverra.igor.terms.core :as terms.core]))

(defrecord TermImage [bind-sym argv]
  protocols/IExpress
  (write [_self] (let [[local-decision set-expr generator-expr] argv]
                   (list
                    'image
                    [bind-sym
                     (protocols/write set-expr)]
                    (clojure.walk/postwalk
                     (fn [e]
                       (if
                        (= e (protocols/write local-decision))
                         bind-sym
                         e))
                     (protocols/write generator-expr)))))
  (codomain [self] {types/Set self})
  (domainv [self] [{types/Numeric self} {types/Set self} {types/Numeric self}])
  (decisions [self] (dissoc
                     (api/unify-argv-decisions self)
                     (first argv)))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (>>
                     {:local-decision (protocols/translate (first argv))
                      :set-expr (protocols/translate (second argv))
                      :generator-expr (protocols/translate (last argv))}
                     "{ {{generator-expr}} | {{local-decision}} in {{set-expr}} }"))
  (evaluate [self solution]
    (let [[local-decision set-expr generator-expr] argv
          s (api/eval-arg set-expr solution)]
      (into (sorted-set)
            (map (fn [elem]
                   (api/eval-arg generator-expr (assoc solution local-decision elem)))
                 s)))))

(defrecord TermImplies [argv]
  protocols/IExpress
  (write [_self] (apply list 'implies (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (take 2 (repeat {types/Bool self})))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (apply
                     api/translate-binary-operation
                     "->"
                     (map protocols/translate (:argv self))))
  (evaluate [self solution]
    (let [[test body] (api/eval-argv self solution)]
      (clojure.core/or (clojure.core/not test) body))))

;; --- Constructor functions ---

(defn- fresh
  ([] (fresh (str (gensym))))
  ([id] (api/->Decision id)))

(defn image [set-expr generator-fn]
  (let [local-decision (api/lexical (fresh))]
    (api/cacheing-validate
     (->TermImage (:id local-decision)
                  [local-decision set-expr (generator-fn local-decision)]))))

(defn implies* [test body]
  (if (clojure.core/every? terms.core/ground? [test body])
    (protocols/evaluate (->TermImplies [test body]) {})
    (api/cacheing-validate (->TermImplies [test body]))))
