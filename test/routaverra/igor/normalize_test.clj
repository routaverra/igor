(ns routaverra.igor.normalize-test
  (:require [clojure.test :refer [deftest is testing]]
            [routaverra.igor :as i]
            [routaverra.igor.normalize :refer [normalize]])
  (:import [routaverra.igor.terms.core TermAnd TermOr]))

;; ----------------------------------------------------------------------
;; Idempotence
;; ----------------------------------------------------------------------

(deftest idempotence-and-test
  (testing "(and a a) normalizes to a"
    (let [x (i/fresh-int (range 10))
          a (i/> x 5)]
      (is (= a (normalize (i/and a a)))))))

(deftest idempotence-or-test
  (testing "(or a a) normalizes to a"
    (let [x (i/fresh-int (range 10))
          a (i/> x 5)]
      (is (= a (normalize (i/or a a)))))))

;; ----------------------------------------------------------------------
;; Commutativity
;; ----------------------------------------------------------------------

(deftest commutativity-and-test
  (testing "(and a b) and (and b a) normalize to the same form"
    (let [x (i/fresh-int (range 10))
          y (i/fresh-int (range 10))
          a (i/> x 5)
          b (i/< y 5)]
      (is (= (normalize (i/and a b))
             (normalize (i/and b a)))))))

(deftest commutativity-or-test
  (testing "(or a b) and (or b a) normalize to the same form"
    (let [x (i/fresh-int (range 10))
          y (i/fresh-int (range 10))
          a (i/> x 5)
          b (i/< y 5)]
      (is (= (normalize (i/or a b))
             (normalize (i/or b a)))))))

(deftest commutativity-three-args-test
  (testing "all permutations of three conjuncts normalize to the same form"
    (let [x (i/fresh-int (range 10))
          y (i/fresh-int (range 10))
          z (i/fresh-int (range 10))
          a (i/> x 5)
          b (i/< y 5)
          c (i/= z 3)
          forms (for [perm [[a b c] [a c b] [b a c] [b c a] [c a b] [c b a]]]
                  (normalize (apply i/and perm)))]
      (is (apply = forms)))))

;; ----------------------------------------------------------------------
;; Identity
;; ----------------------------------------------------------------------

(deftest identity-and-true-test
  (testing "(and a true) normalizes to a"
    (let [x (i/fresh-int (range 10))
          a (i/> x 5)]
      (is (= a (normalize (i/and a true)))))))

(deftest identity-or-false-test
  (testing "(or a false) normalizes to a"
    (let [x (i/fresh-int (range 10))
          a (i/> x 5)]
      (is (= a (normalize (i/or a false)))))))

;; ----------------------------------------------------------------------
;; Annihilation
;; ----------------------------------------------------------------------

(deftest annihilation-and-false-test
  (testing "(and a false) normalizes to false"
    (let [x (i/fresh-int (range 10))
          a (i/> x 5)]
      (is (= false (normalize (i/and a false)))))))

(deftest annihilation-or-true-test
  (testing "(or a true) normalizes to true"
    (let [x (i/fresh-int (range 10))
          a (i/> x 5)]
      (is (= true (normalize (i/or a true)))))))

;; ----------------------------------------------------------------------
;; Absorption
;; ----------------------------------------------------------------------

(deftest absorption-and-test
  (testing "(and a (or a b)) normalizes to a"
    (let [x (i/fresh-int (range 10))
          y (i/fresh-int (range 10))
          a (i/> x 5)
          b (i/< y 5)]
      (is (= a (normalize (i/and a (i/or a b))))))))

(deftest absorption-or-test
  (testing "(or a (and a b)) normalizes to a"
    (let [x (i/fresh-int (range 10))
          y (i/fresh-int (range 10))
          a (i/> x 5)
          b (i/< y 5)]
      (is (= a (normalize (i/or a (i/and a b))))))))

;; ----------------------------------------------------------------------
;; Flattening
;; ----------------------------------------------------------------------

(deftest flattening-and-test
  (testing "(and a (and b c)) and (and a b c) normalize to the same form"
    (let [x (i/fresh-int (range 10))
          y (i/fresh-int (range 10))
          z (i/fresh-int (range 10))
          a (i/> x 5)
          b (i/< y 5)
          c (i/= z 3)]
      (is (= (normalize (i/and a (i/and b c)))
             (normalize (i/and a b c)))))))

(deftest flattening-or-test
  (testing "(or a (or b c)) and (or a b c) normalize to the same form"
    (let [x (i/fresh-int (range 10))
          y (i/fresh-int (range 10))
          z (i/fresh-int (range 10))
          a (i/> x 5)
          b (i/< y 5)
          c (i/= z 3)]
      (is (= (normalize (i/or a (i/or b c)))
             (normalize (i/or a b c)))))))

;; ----------------------------------------------------------------------
;; Recursion through non-and/or nodes
;; ----------------------------------------------------------------------

(deftest recursion-through-if-test
  (testing "and/or inside an if test still gets normalized"
    (let [x (i/fresh-int (range 10))
          y (i/fresh-int (range 10))
          a (i/> x 5)
          b (i/< y 5)
          form-1 (i/if (i/and a b) 1 2)
          form-2 (i/if (i/and b a) 1 2)]
      (is (= (normalize form-1) (normalize form-2))))))

;; ----------------------------------------------------------------------
;; Pass-through (non-and/or nodes round-trip structurally)
;; ----------------------------------------------------------------------

(deftest pass-through-equals-test
  (testing "TermEquals passes through normalize unchanged structurally"
    (let [x (i/fresh-int (range 10))
          e (i/= x 5)]
      (is (= e (normalize e))))))

(deftest pass-through-decision-test
  (testing "Decision leaf passes through normalize unchanged"
    (let [x (i/fresh-int (range 10))]
      (is (= x (normalize x))))))

(deftest pass-through-primitive-test
  (testing "primitives pass through normalize unchanged"
    (is (= 5 (normalize 5)))
    (is (= true (normalize true)))
    (is (= false (normalize false)))
    (is (= :foo (normalize :foo)))))

;; ----------------------------------------------------------------------
;; Stability
;; ----------------------------------------------------------------------

(deftest stability-test
  (testing "normalize is idempotent: (normalize (normalize x)) = (normalize x)"
    (let [x (i/fresh-int (range 10))
          y (i/fresh-int (range 10))
          a (i/> x 5)
          b (i/< y 5)
          forms [a
                 (i/and a b)
                 (i/or a (i/and b a))
                 (i/and a (i/or b a))
                 (i/and a b a)]]
      (doseq [f forms]
        (is (= (normalize f) (normalize (normalize f)))
            (str "normalize should be idempotent on " (pr-str f)))))))

;; ----------------------------------------------------------------------
;; Result type sanity
;; ----------------------------------------------------------------------

(deftest result-type-test
  (testing "normalize result is a TermAnd when multiple distinct conjuncts remain"
    (let [x (i/fresh-int (range 10))
          y (i/fresh-int (range 10))
          a (i/> x 5)
          b (i/< y 5)
          result (normalize (i/and a b))]
      (is (instance? TermAnd result))
      (is (= 2 (count (:argv result))))))
  (testing "normalize collapses to the single child when only one remains"
    (let [x (i/fresh-int (range 10))
          a (i/> x 5)]
      (is (= a (normalize (i/and a a a))))
      (is (= a (normalize (i/and a true true)))))))
