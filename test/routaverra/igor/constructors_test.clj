(ns routaverra.igor.constructors-test
  (:require [clojure.test :refer [deftest is testing]]
            [routaverra.igor :as i]))

(deftest domain-int-test
  (testing "domain over an int collection produces an int var that solves"
    (let [x (i/domain (range 10))
          sol (i/satisfy (i/and (i/>= x 3) (i/<= x 5)))]
      (is (some? sol))
      (is (<= 3 (get sol x) 5)))))

(deftest domain-keyword-test
  (testing "domain over a keyword collection produces a keyword var that solves"
    (let [x (i/domain #{:red :blue :green})
          sol (i/satisfy (i/= x :blue))]
      (is (some? sol))
      (is (= :blue (get sol x))))))

(deftest universe-int-test
  (testing "universe over ints solves"
    (let [s (i/universe (range 5))
          sol (i/satisfy (i/and (i/contains? s 1) (i/contains? s 3)))]
      (is (some? sol))
      (is (clojure.set/subset? #{1 3} (get sol s))))))

(deftest universe-keyword-test
  (testing "universe over keywords solves"
    (let [s (i/universe #{:a :b :c})
          sol (i/satisfy (i/contains? s :b))]
      (is (some? sol))
      (is (contains? (get sol s) :b)))))

(deftest bool-test
  (testing "bool var solves"
    (let [b (i/bool)
          sol (i/satisfy (i/true? b))]
      (is (some? sol))
      (is (true? (get sol b))))))

(deftest type-rescue-empty-int
  (testing ":type :int rescues an empty/computed collection"
    (let [x (i/domain (filter even? []) {:type :int})
          sol (i/satisfy (i/= x 0))]
      ;; The bind range is empty; no solution should exist, but
      ;; construction must succeed without throwing.
      (is (nil? sol)))))

(deftest type-rescue-empty-keyword
  (testing ":type :keyword rescues an empty/computed collection"
    (is (some? (i/domain [] {:type :keyword})))
    (is (some? (i/universe [] {:type :keyword})))))

(deftest empty-no-type-error
  (testing "empty coll without :type throws the documented message"
    (let [e (try (i/domain []) nil (catch Exception e e))]
      (is (some? e))
      (is (= "Cannot infer element type from empty domain; pass {:type :int} or {:type :keyword}"
             (ex-message e))))
    (let [e (try (i/universe []) nil (catch Exception e e))]
      (is (some? e))
      (is (= "Cannot infer element type from empty domain; pass {:type :int} or {:type :keyword}"
             (ex-message e))))))

(deftest mixed-type-error
  (testing "mixed-type coll throws the documented message"
    (let [e (try (i/domain #{1 :red}) nil (catch Exception e e))]
      (is (some? e))
      (is (= "Heterogeneous domains are not supported; all elements must share a type"
             (ex-message e))))
    (let [e (try (i/universe [:a 2 :b]) nil (catch Exception e e))]
      (is (some? e))
      (is (= "Heterogeneous domains are not supported; all elements must share a type"
             (ex-message e))))))

(deftest declared-type-mismatch-error
  (testing "declared :type contradicting coll throws the documented message"
    (let [e (try (i/domain [1 2 3] {:type :keyword}) nil (catch Exception e e))]
      (is (some? e))
      (is (= "Domain values are inconsistent with declared :type"
             (ex-message e))))
    (let [e (try (i/domain [:a :b] {:type :int}) nil (catch Exception e e))]
      (is (some? e))
      (is (= "Domain values are inconsistent with declared :type"
             (ex-message e))))
    (let [e (try (i/universe [1 2] {:type :keyword}) nil (catch Exception e e))]
      (is (some? e))
      (is (= "Domain values are inconsistent with declared :type"
             (ex-message e))))))

(deftest declared-type-invalid-value-error
  (testing "an unknown :type value also surfaces the inconsistency error"
    (let [e (try (i/domain [1 2] {:type :bogus}) nil (catch Exception e e))]
      (is (some? e))
      (is (= "Domain values are inconsistent with declared :type"
             (ex-message e))))))
