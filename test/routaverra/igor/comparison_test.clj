(ns routaverra.igor.comparison-test
  (:require [clojure.test :refer [deftest is testing]]
            [routaverra.igor :as i]))

;; ----------------------------------------------------------------------
;; equivalent?
;; ----------------------------------------------------------------------

(deftest equivalent-commutative-test
  (testing "(and a b) and (and b a) are equivalent"
    (let [x (i/domain (range 10))
          y (i/domain (range 10))
          a (i/> x 5)
          b (i/< y 5)]
      (is (i/equivalent? (i/and a b) (i/and b a))))))

(deftest equivalent-idempotent-test
  (testing "(and a a) and a are equivalent"
    (let [x (i/domain (range 10))
          a (i/> x 5)]
      (is (i/equivalent? (i/and a a) a)))))

(deftest equivalent-distinct-test
  (testing "distinct atomic constraints are not equivalent"
    (let [x (i/domain (range 10))]
      (is (not (i/equivalent? (i/> x 5) (i/< x 5)))))))

(deftest equivalent-absorption-test
  (testing "(and a (or a b)) and a are equivalent via absorption"
    (let [x (i/domain (range 10))
          y (i/domain (range 10))
          a (i/> x 5)
          b (i/< y 5)]
      (is (i/equivalent? (i/and a (i/or a b)) a)))))

(deftest equivalent-flatten-test
  (testing "nested ands flatten before comparison"
    (let [x (i/domain (range 10))
          y (i/domain (range 10))
          z (i/domain (range 10))
          a (i/> x 5)
          b (i/< y 5)
          c (i/= z 3)]
      (is (i/equivalent? (i/and a (i/and b c))
                         (i/and (i/and a b) c))))))

;; ----------------------------------------------------------------------
;; stricter?
;; ----------------------------------------------------------------------

(deftest stricter-reflexive-test
  (testing "every constraint is stricter-or-equal to itself"
    (let [x (i/domain (range 10))
          c (i/and (i/> x 0) (i/< x 5))]
      (is (i/stricter? c c)))))

(deftest stricter-conjunct-membership-test
  (testing "(and a b) is stricter than each conjunct"
    (let [x (i/domain (range 10))
          y (i/domain (range 10))
          a (i/> x 0)
          b (i/< y 5)]
      (is (i/stricter? (i/and a b) a))
      (is (i/stricter? (i/and a b) b)))))

(deftest stricter-conjunct-non-implication-test
  (testing "a is not stricter than (and a b)"
    (let [x (i/domain (range 10))
          y (i/domain (range 10))
          a (i/> x 0)
          b (i/< y 5)]
      (is (not (i/stricter? a (i/and a b)))))))

(deftest stricter-disjunct-membership-test
  (testing "a is stricter than (or a b)"
    (let [x (i/domain (range 10))
          y (i/domain (range 10))
          a (i/> x 0)
          b (i/< y 5)]
      (is (i/stricter? a (i/or a b))))))

(deftest stricter-conjunct-subset-test
  (testing "(and a b c) is stricter than (and a c)"
    (let [x (i/domain (range 10))
          y (i/domain (range 10))
          z (i/domain (range 10))
          a (i/> x 0)
          b (i/< y 5)
          c (i/= z 3)]
      (is (i/stricter? (i/and a b c) (i/and a c))))))

(deftest stricter-domain-greater-test
  (testing "(> x 10) is stricter than (> x 5)"
    (let [x (i/domain (range 100))]
      (is (i/stricter? (i/> x 10) (i/> x 5)))
      (is (not (i/stricter? (i/> x 5) (i/> x 10)))))))

(deftest stricter-domain-less-test
  (testing "(< x 5) is stricter than (< x 10)"
    (let [x (i/domain (range 100))]
      (is (i/stricter? (i/< x 5) (i/< x 10)))
      (is (not (i/stricter? (i/< x 10) (i/< x 5)))))))

(deftest stricter-domain-mixed-ops-test
  (testing "(> x 10) is stricter than (>= x 10)"
    (let [x (i/domain (range 100))]
      (is (i/stricter? (i/> x 10) (i/>= x 10))))))

(deftest stricter-domain-equality-implies-test
  (testing "(= x 7) is stricter than (> x 5) and (< x 10)"
    (let [x (i/domain (range 100))]
      (is (i/stricter? (i/= x 7) (i/> x 5)))
      (is (i/stricter? (i/= x 7) (i/< x 10)))
      (is (not (i/stricter? (i/= x 7) (i/> x 10)))))))

(deftest stricter-different-variables-no-implication-test
  (testing "comparisons over different variables don't imply each other"
    (let [x (i/domain (range 100))
          y (i/domain (range 100))]
      (is (not (i/stricter? (i/> x 5) (i/> y 5)))))))

(deftest stricter-conservative-on-hard-cases-test
  (testing "hard cross-variable arithmetic returns false (conservative)"
    (let [x (i/domain (range 10))
          y (i/domain (range 10))]
      ;; True in reality but not provable structurally
      (is (not (i/stricter? (i/and (i/= x 0) (i/= y 0))
                            (i/= (i/+ x y) 0)))))))
