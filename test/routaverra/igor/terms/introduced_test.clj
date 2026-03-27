(ns routaverra.igor.terms.introduced-test
  (:require [clojure.test :refer [deftest is testing]]
            [routaverra.igor :as i]
            [routaverra.igor.protocols :as protocols]
            [routaverra.igor.utils.test :refer [throws?]]))

(deftest every?-test
  (testing "every?"
    (testing "internal decision is validated as numeric"
      (is (= true
             (throws?
              (i/every? (i/fresh) (fn [a] (i/= a #{}))))))

      (is (= true
             (throws?
              (i/every? (i/fresh) (fn [a] (i/contains? a 1)))))))

    (testing "internal decision is hidden from external retrieval"
      (is (= 1
             (count
              (protocols/decisions
               (i/every? (i/fresh) (fn [a] (i/= a 1))))))))

    (testing "every? evaluates"
      (let [x (i/fresh)
            res (i/satisfy
                 (i/every? (i/bind (range 100) x)
                   (fn [a] (i/= 5 (i/mod a 12)))))]
        (is (= #{65 77 41 89 29 17 5 53}
               (get res x)))))

    (testing "cluster-free constraint"
      (let [cluster-free (fn [set-decision]
                           (i/every? (i/bind (range 12) set-decision)
                             (fn [a]
                               (i/implies (i/contains? set-decision (i/mod (i/+ a 1) 12))
                                 (i/not (i/contains? set-decision (i/mod (i/+ a 2) 12)))))))
            x (i/fresh)
            res (i/satisfy
                 (cluster-free x))
            validate (fn [s]
                       (every?
                        clojure.core/true?
                        (for [e s]
                          (if (clojure.core/contains? s (clojure.core/mod (clojure.core/+ e 1) 12))
                            (not (clojure.core/contains? s (clojure.core/mod (clojure.core/+ e 2) 12)))
                            true))))]
        (is (= true (validate (get res x))))))))

(deftest some-test
  (testing "some"
    (testing "internal decision is hidden from external retrieval"
      (is (= 1
             (count
              (protocols/decisions
               (i/some (i/fresh) (fn [a] (i/= a 1))))))))

    (testing "some constrains: at least one element satisfies"
      (let [x (i/fresh-set (range 10))
            res (i/satisfy
                 (i/and
                  (i/some x (fn [a] (i/> a 7)))
                  (i/<= (i/count x) 2)))]
        (is (clojure.core/some #(> % 7) (get res x)))))

    (testing "some evaluates against solution"
      (let [x (i/fresh-set (range 10))
            term (i/some x (fn [a] (i/> a 5)))]
        (is (true? (protocols/evaluate term {x #{1 2 8}})))
        (is (false? (protocols/evaluate term {x #{1 2 3}})))))

    (testing "some returns false for empty set"
      (let [x (i/fresh-set (range 5))
            term (i/some x (fn [a] (i/> a 0)))]
        (is (false? (protocols/evaluate term {x #{}})))))))

(deftest image-test
  (testing "image"
    (testing "internal decision is validated as numeric"
      (is (= true
             (throws?
              (i/image (i/fresh) (fn [a] (i/if (i/= a #{}) 1 2)))))))

    (testing "internal decision is hidden from external retrieval"
      (is (= 1
             (count
              (protocols/decisions
               (i/image (i/fresh) (fn [a] (i/+ a 1))))))))

    (testing "image evaluates"
      (let [x (i/fresh)
            res (i/satisfy
                 (i/=
                  #{1 2 3}
                  (i/image (i/bind (range 12) x)
                    (fn [a] (i/+ a 1)))))]
        (is (= #{0 1 2} (get res x)))))))
