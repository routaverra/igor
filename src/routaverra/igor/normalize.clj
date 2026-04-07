(ns routaverra.igor.normalize
  "AST normalization via lattice laws.

   `normalize` rewrites a constraint AST so that semantically-identical
   compositions hash to identical ASTs. The rewrites are:

   - Flatten nested same-op ands/ors
   - Annihilation: (and ... false ...) → false; (or ... true ...) → true
   - Identity: drop true from and; drop false from or
   - Idempotence: dedupe children
   - Absorption: drop (or x y) from an and that already contains x; mirror for or
   - Commutativity: sort children by a stable canonical key
   - Collapse: 0 children → identity element; 1 child → that child unwrapped

   The walk is post-order so each parent sees normalized children.
   TermAnd / TermOr nodes get the rewrites; every other record type with
   `:argv` is rebuilt with normalized children but otherwise unchanged
   (argument order is semantically meaningful for non-lattice nodes).
   Leaves (Decision, primitives, sets, opaque records) pass through."
  (:require [routaverra.igor.canonical :as canonical]
            [routaverra.igor.terms.core :as terms])
  (:import [routaverra.igor.terms.core TermAnd TermOr]))

(defn- node-key
  "Stable, total sort key for a normalized child. Uses
   `canonical/canonical-form` (which replaces gensym Decision IDs with
   depth-first encounter indices) so structurally-identical children
   produce identical keys."
  [node]
  (pr-str (canonical/canonical-form node)))

(declare normalize)

(defn- and-rewrite
  "Apply the and-side lattice rewrites to an already-recursively-normalized
   sequence of children."
  [children]
  (let [flattened (mapcat (fn [c]
                            (if (instance? TermAnd c)
                              (:argv c)
                              [c]))
                          children)]
    (if (some false? flattened)
      false
      (let [non-identity (remove true? flattened)
            deduped      (distinct non-identity)
            sorted       (sort-by node-key deduped)
            ;; Absorption: drop any (or x y ...) child whose argv intersects
            ;; the and-level child set.
            and-set      (set sorted)
            absorbed     (remove (fn [c]
                                   (and (instance? TermOr c)
                                        (some and-set (:argv c))))
                                 sorted)]
        (case (count absorbed)
          0 true
          1 (first absorbed)
          (terms/->TermAnd (vec absorbed)))))))

(defn- or-rewrite
  "Apply the or-side lattice rewrites to an already-recursively-normalized
   sequence of children."
  [children]
  (let [flattened (mapcat (fn [c]
                            (if (instance? TermOr c)
                              (:argv c)
                              [c]))
                          children)]
    (if (some true? flattened)
      true
      (let [non-identity (remove false? flattened)
            deduped      (distinct non-identity)
            sorted       (sort-by node-key deduped)
            or-set       (set sorted)
            absorbed     (remove (fn [c]
                                   (and (instance? TermAnd c)
                                        (some or-set (:argv c))))
                                 sorted)]
        (case (count absorbed)
          0 false
          1 (first absorbed)
          (terms/->TermOr (vec absorbed)))))))

(defn normalize
  "Rewrite a constraint AST into canonical form via lattice laws.

   Post-order: children are normalized first, then the parent's rewrites
   apply. TermAnd / TermOr get the full lattice rewrites (flatten,
   annihilation, identity, dedup, absorption, sort, collapse). Every
   other record with `:argv` is rebuilt with normalized children but
   otherwise unchanged. Leaves pass through.

   Pure: never invokes the solver, never validates, never mutates."
  [expr]
  (cond
    (instance? TermAnd expr)
    (and-rewrite (mapv normalize (:argv expr)))

    (instance? TermOr expr)
    (or-rewrite (mapv normalize (:argv expr)))

    (and (record? expr) (contains? expr :argv))
    (assoc expr :argv (mapv normalize (:argv expr)))

    :else expr))
