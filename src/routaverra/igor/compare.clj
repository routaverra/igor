(ns routaverra.igor.compare
  "Structural comparison of constraints under the lattice on solution sets.

   These functions decide *easy* cases without invoking the solver. They
   operate on normalized ASTs (each input is normalized internally).

   - `equivalent?` is sound and complete for the lattice rewrites that
     `normalize` performs (commutativity, idempotence, absorption,
     identity, annihilation). Constraints that are equivalent for deeper
     reasons (e.g., arithmetic identities) may still compare unequal.

   - `stricter?` is sound but conservative: a `true` return is always
     correct; a `false` return means \"not provable structurally\", not
     a counter-example. Callers needing a complete decision can fall
     back to solving `(i/and c1 (i/not c2))` for unsatisfiability."
  (:require [routaverra.igor.normalize :as normalize])
  (:import [routaverra.igor.terms.core
            TermAnd TermOr
            TermGreaterThan TermLessThan
            TermGreaterThanOrEqualTo TermLessThanOrEqualTo
            TermEquals]))

(defn equivalent?
  "True iff c1 and c2 denote the same constraint, decided structurally
   via normalization. Sound and complete for the lattice rewrites in
   `normalize`."
  [c1 c2]
  (= (normalize/normalize c1)
     (normalize/normalize c2)))

;; ----------------------------------------------------------------------
;; Domain-inclusion: same-variable comparisons against integer constants
;; ----------------------------------------------------------------------

(def ^:private comparison-types
  #{TermGreaterThan TermLessThan TermGreaterThanOrEqualTo TermLessThanOrEqualTo TermEquals})

(defn- comparison? [x]
  (and x (contains? comparison-types (class x))))

(defn- binary-comparison
  "If `expr` is a binary comparison whose left side is a Decision and
   whose right side is a number, return [op decision constant]; else nil.
   `op` is one of :gt :lt :ge :le :eq."
  [expr]
  (when (comparison? expr)
    (let [[a b] (:argv expr)]
      (when (and (= 2 (count (:argv expr)))
                 (instance? routaverra.igor.api.Decision a)
                 (number? b))
        [(condp instance? expr
           TermGreaterThan          :gt
           TermLessThan             :lt
           TermGreaterThanOrEqualTo :ge
           TermLessThanOrEqualTo    :le
           TermEquals               :eq)
         a
         b]))))

(defn- domain-stricter?
  "True if c1 implies c2 by same-variable, integer-bound comparison.
   Both must match `binary-comparison` against the same Decision."
  [c1 c2]
  (when-let [[op1 d1 k1] (binary-comparison c1)]
    (when-let [[op2 d2 k2] (binary-comparison c2)]
      (when (= d1 d2)
        (case [op1 op2]
          ;; > x k1 implies > x k2  iff  k1 >= k2
          [:gt :gt] (>= k1 k2)
          [:gt :ge] (>= k1 k2) ;; > k1 implies >= k2 iff every n>k1 satisfies n>=k2
          ;; < x k1 implies < x k2  iff  k1 <= k2
          [:lt :lt] (<= k1 k2)
          [:lt :le] (<= k1 k2)
          ;; >= x k1 implies >= x k2  iff  k1 >= k2
          [:ge :ge] (>= k1 k2)
          [:ge :gt] (> k1 k2)
          ;; <= x k1 implies <= x k2  iff  k1 <= k2
          [:le :le] (<= k1 k2)
          [:le :lt] (< k1 k2)
          ;; = x k1 implies anything iff k1 satisfies the rhs predicate
          [:eq :gt] (> k1 k2)
          [:eq :ge] (>= k1 k2)
          [:eq :lt] (< k1 k2)
          [:eq :le] (<= k1 k2)
          [:eq :eq] (= k1 k2)
          nil)))))

;; ----------------------------------------------------------------------
;; stricter?
;; ----------------------------------------------------------------------

(declare stricter?*)

(defn- and? [x] (instance? TermAnd x))
(defn- or?  [x] (instance? TermOr  x))

(defn- stricter?*
  "Sound, conservative implication check on already-normalized inputs."
  [n1 n2]
  (cond
    ;; Trivial: identical normalized forms
    (= n1 n2) true

    ;; ⊥ implies anything
    (false? n1) true
    ;; anything implies ⊤
    (true? n2) true

    ;; n1 is a meet that contains n2 directly (or recursively-implies one of n2's conjuncts)
    (and? n1)
    (let [conjuncts (set (:argv n1))]
      (cond
        ;; n2 is one of n1's conjuncts → n1 implies it
        (contains? conjuncts n2) true

        ;; both are meets: every conjunct of n2 must be implied by n1
        (and? n2)
        (every? (fn [c2-i]
                  (or (contains? conjuncts c2-i)
                      (some #(stricter?* % c2-i) conjuncts)))
                (:argv n2))

        ;; some conjunct of n1 is itself stricter than n2
        :else
        (boolean (some #(stricter?* % n2) conjuncts))))

    ;; n2 is a join that contains n1 directly (or contains a disjunct that n1 implies)
    (or? n2)
    (let [disjuncts (set (:argv n2))]
      (cond
        (contains? disjuncts n1) true
        :else
        (boolean (some #(stricter?* n1 %) disjuncts))))

    ;; n1 is a join: every branch of n1 must imply n2
    (or? n1)
    (every? #(stricter?* % n2) (:argv n1))

    ;; Same-variable, integer-bound comparison
    :else
    (boolean (domain-stricter? n1 n2))))

(defn stricter?
  "True iff c1 implies c2 — i.e., every solution to c1 is also a solution
   to c2 — decided structurally on normalized forms.

   Sound: when this returns true, c1 really does imply c2.
   Conservative: a false return means \"not provable structurally\", not
   a counter-example. For a complete decision, fall back to solving
   `(i/and c1 (i/not c2))` and checking unsatisfiability."
  [c1 c2]
  (stricter?* (normalize/normalize c1) (normalize/normalize c2)))
