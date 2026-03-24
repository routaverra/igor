(ns routaverra.igor.alternatives-test
  (:require [clojure.test :refer [deftest is testing]]
            [routaverra.igor :as i]))

(deftest alternatives-basic-test
  (testing "solver picks one of two branches"
    (let [domain #{1 2 3 4 5}
          x (i/fresh-int domain)
          y (i/fresh-int domain)
          alt (i/alternatives (i/= x 1) (i/= y 5))
          sol0 (i/satisfy (i/and alt (i/= (:choice alt) 0)))
          sol1 (i/satisfy (i/and alt (i/= (:choice alt) 1)))]
      (is (= 0 (i/chosen alt sol0)))
      (is (= 1 (i/resolve sol0 x)))
      (is (= 1 (i/chosen alt sol1)))
      (is (= 5 (i/resolve sol1 y))))))

(deftest alternatives-free-choice-test
  (testing "solver freely chooses among branches"
    (let [x (i/fresh-int (set (range 1 11)))
          y (i/fresh-int (set (range 1 11)))
          alt (i/alternatives (i/> x 8) (i/< y 3))
          sol (i/satisfy alt)
          idx (i/chosen alt sol)]
      (is (#{0 1} idx))
      (if (= idx 0)
        (is (> (i/resolve sol x) 8))
        (is (< (i/resolve sol y) 3))))))

(deftest alternatives-three-branches-test
  (testing "three-way choice"
    (let [domain #{1 2 3 4 5 6 7 8 9}
          a (i/fresh-int domain)
          b (i/fresh-int domain)
          c (i/fresh-int domain)
          alt (i/alternatives (i/= a 1) (i/= b 5) (i/= c 9))]
      (doseq [k [0 1 2]]
        (let [sol (i/satisfy (i/and alt (i/= (:choice alt) k)))]
          (is (= (get [1 5 9] k)
                 (i/resolve sol (nth [a b c] k)))))))))

(deftest alternatives-cross-branch-constraints-test
  (testing "constraints can relate variables across two alternatives"
    (let [domain (set (range 1 11))
          x1 (i/fresh-int domain)
          x2 (i/fresh-int domain)
          y1 (i/fresh-int domain)
          y2 (i/fresh-int domain)
          alt1 (i/alternatives (i/>= x1 1) (i/>= x2 1))
          alt2 (i/alternatives (i/>= y1 1) (i/>= y2 1))
          sol (i/satisfy (i/and alt1 alt2
                                (i/= (:choice alt1) 0)
                                (i/= (:choice alt2) 0)
                                (i/= (i/+ x1 y1) 7)))]
      (is (= 7 (clojure.core/+ (i/resolve sol x1)
                                (i/resolve sol y1)))))))

(deftest alternatives-nested-test
  (testing "alternatives can nest — inner choice within outer branch"
    (let [domain #{1 2 3 4 5 6 7 8 9}
          a (i/fresh-int domain)
          b (i/fresh-int domain)
          c (i/fresh-int domain)
          inner (i/alternatives (i/= a 3) (i/= b 7))
          outer (i/alternatives inner (i/= c 1))
          sol (i/satisfy (i/and outer
                                (i/= (:choice outer) 0)
                                (i/= (:choice inner) 1)))]
      (is (= 7 (i/resolve sol b))))))

(deftest alternatives-as-direct-constraint-test
  (testing "alternatives handle works directly in i/satisfy without wrapping"
    (let [x (i/fresh-int #{1 2 3})
          y (i/fresh-int #{4 5 6})
          alt (i/alternatives (i/= x 2) (i/= y 5))
          sol (i/satisfy alt)
          idx (i/chosen alt sol)]
      (is (some? sol))
      (if (= idx 0)
        (is (= 2 (i/resolve sol x)))
        (is (= 5 (i/resolve sol y)))))))

(deftest resolve-walks-nested-forms-test
  (testing "i/resolve replaces decision vars in nested data structures"
    (let [x (i/fresh-int #{1 2 3})
          y (i/fresh-int #{4 5 6})
          alt (i/alternatives (i/and (i/= x 2) (i/= y 5)))
          sol (i/satisfy alt)
          form {:pitch x :nested [{:dur 4 :vel y}]}
          resolved (i/resolve sol form)]
      (is (= 2 (:pitch resolved)))
      (is (= 5 (:vel (first (:nested resolved))))))))

(deftest resolve-evaluates-term-expressions-test
  (testing "i/resolve collapses igor terms to concrete values"
    (let [x (i/fresh-int #{10 20 30})
          sol (i/satisfy (i/= x 20))
          form {:a (i/+ x 5)
                :b (i/- x 3)
                :c (i/* x 2)}
          resolved (i/resolve sol form)]
      (is (= 25 (:a resolved)))
      (is (= 17 (:b resolved)))
      (is (= 40 (:c resolved)))))
  (testing "nested term expressions collapse fully"
    (let [x (i/fresh-int #{1 2 3})
          sol (i/satisfy (i/= x 2))
          form {:val (i/+ (i/* x 10) 7)}
          resolved (i/resolve sol form)]
      (is (= 27 (:val resolved)))))
  (testing "terms in vectors and nested maps"
    (let [x (i/fresh-int #{5 10 15})
          y (i/fresh-int #{1 2 3})
          sol (i/satisfy (i/and (i/= x 10) (i/= y 2)))
          form [(i/+ x y) {:inner (i/max x y)}]
          resolved (i/resolve sol form)]
      (is (= 12 (first resolved)))
      (is (= 10 (:inner (second resolved)))))))
