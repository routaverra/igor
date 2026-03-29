(ns routaverra.igor.alternatives
  "Channeled disjunction — an indicator variable selects one of N
   constraint branches.

   Returns a handle that IS the constraint (implements IExpress) and
   supports a choice extractor like the graph API."
  (:require [routaverra.igor.protocols :as protocols]
            [routaverra.igor.api :as api]
            [routaverra.igor.terms.core :as terms]
            [routaverra.igor.terms.introduced :as terms.introduced]))

(defrecord Alternatives [choice-var branches constraint-expr]
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
     - Supports (i/choice handle) to get the decision variable for the selected branch index"
  [& constraints]
  (let [constraints (vec constraints)
        n           (count constraints)
        choice-var  (api/fresh-int (set (range n)))
        constraint  (apply terms/and*
                      (map-indexed
                        (fn [k c]
                          (terms.introduced/?>* (terms/equals choice-var k) c))
                        constraints))]
    (->Alternatives choice-var constraints constraint)))

(defn choice
  "Returns the choice decision variable from an alternatives handle.
   Use this to build constraints over the selection, or resolve it against
   a solution to get the index of the branch the solver chose."
  [handle]
  {:pre [(instance? Alternatives handle)]}
  (:choice-var handle))
