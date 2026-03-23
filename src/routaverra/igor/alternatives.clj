(ns routaverra.igor.alternatives
  "Channeled disjunction — an indicator variable selects one of N
   constraint branches.

   Returns a handle that IS the constraint (implements IExpress) and
   supports solution readers (chosen) like the graph API."
  (:require [routaverra.igor.protocols :as protocols]
            [routaverra.igor.api :as api]
            [routaverra.igor.terms.core :as terms]))

(defrecord Alternatives [choice branches constraint-expr]
  protocols/IExpress
  (write [_self] (protocols/write constraint-expr))
  (codomain [_self] (protocols/codomain constraint-expr))
  (domainv [_self] (protocols/domainv constraint-expr))
  (decisions [_self] (api/cacheing-decisions constraint-expr))
  (bindings [_self] (protocols/bindings constraint-expr))
  (validate [self] (api/cacheing-validate constraint-expr) self)
  (translate [_self] (protocols/translate constraint-expr))
  (evaluate [_self solution] (protocols/evaluate constraint-expr solution)))

(defn alternatives
  "Channeled disjunction: an indicator variable selects one of N
   constraint branches.

   Each argument is a constraint expression. Returns a handle that:
     - IS the constraint (pass directly to i/satisfy, i/and, etc.)
     - Has a :choice decision variable (the index of the chosen branch)
     - Supports i/chosen to read which branch was selected"
  [& constraints]
  (let [constraints (vec constraints)
        n           (count constraints)
        choice      (api/fresh-int (set (range n)))
        constraint  (apply terms/and*
                      (map-indexed
                        (fn [k c]
                          (terms/when* (terms/equals choice k) c))
                        constraints))]
    (->Alternatives choice constraints constraint)))

(defn chosen
  "Returns the index of the chosen branch from a solution."
  [handle solution]
  {:pre [(instance? Alternatives handle)]}
  (get solution (:choice handle)))
