(ns routaverra.igor.composability-test
  (:require [clojure.test :refer [deftest is testing]]
            [routaverra.igor :as i]))

(deftest constraint-producing-functions-test
  (testing "map a constraint-producing function over indices"
    (let [exceeds-index (fn [vars k] (i/> (nth vars k) k))
          n    5
          vars (vec (repeatedly n #(i/fresh-int (range 20))))
          vals (i/solve (apply i/and (map #(exceeds-index vars %) (range n)))
                              vars)]
      (doseq [k (range n)]
        (is (> (nth vals k) k)
            (str "value at index " k " should exceed its index"))))))

(deftest higher-order-constraint-builders-test
  (testing "pairwise combinator for strictly increasing with bounded gaps"
    (let [pairwise (fn [f vars]
                     (apply i/and (map (fn [[a b]] (f a b))
                                       (partition 2 1 vars))))
          vars (vec (repeatedly 5 #(i/fresh-int (range 20))))
          vals (i/solve (i/and (pairwise i/< vars)
                                     (pairwise (fn [a b] (i/<= (i/- b a) 3)) vars))
                              vars)]
      (is (= vals (sort vals)) "sequence should be sorted")
      (doseq [[a b] (partition 2 1 vals)]
        (is (< a b) "consecutive values should be strictly increasing")
        (is (<= (- b a) 3) "gap between consecutive values should be ≤ 3")))))

(deftest composing-constraint-modules-test
  (testing "4×4 Latin square from row and column constraint modules"
    (let [row-constraints (fn [grid]
                            (apply i/and (for [row grid]
                                           (apply i/all-different row))))
          col-constraints (fn [grid]
                            (let [cols (apply map vector grid)]
                              (apply i/and (for [col cols]
                                             (apply i/all-different col)))))
          grid (vec (for [_ (range 4)]
                      (vec (repeatedly 4 #(i/fresh-int (range 1 5))))))
          result (i/solve (i/and (row-constraints grid)
                                       (col-constraints grid))
                                grid)]
      (doseq [row result]
        (is (= #{1 2 3 4} (set row)) "each row should contain {1 2 3 4}"))
      (doseq [col (apply map vector result)]
        (is (= #{1 2 3 4} (set col)) "each column should contain {1 2 3 4}")))))
