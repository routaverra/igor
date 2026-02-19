(ns igor.constraint-problems-test
  "Tests based on classic constraint satisfaction problems,
   inspired by core.logic CLP(FD) patterns and standard CSP benchmarks."
  (:require [clojure.test :refer [deftest is testing]]
            [igor.core :as i]
            [igor.utils.test :refer [only-val throws?]]))

;; ============================================================
;; 1. Classic arithmetic CSPs
;; ============================================================

(deftest send-more-money-test
  (testing "SEND + MORE = MONEY cryptarithmetic puzzle"
    (let [digits-domain (range 10)
          s (i/fresh-int digits-domain "S") e (i/fresh-int digits-domain "E")
          n (i/fresh-int digits-domain "N") d (i/fresh-int digits-domain "D")
          m (i/fresh-int digits-domain "M") o (i/fresh-int digits-domain "O")
          r (i/fresh-int digits-domain "R") y (i/fresh-int digits-domain "Y")
          digits [s e n d m o r y]
          send  (i/+ (i/* s 1000) (i/* e 100) (i/* n 10) d)
          more  (i/+ (i/* m 1000) (i/* o 100) (i/* r 10) e)
          money (i/+ (i/* m 10000) (i/* o 1000) (i/* n 100) (i/* e 10) y)
          ;; all-different via pairwise not=
          all-diff (->> (for [i (range (count digits))
                              j (range (inc i) (count digits))]
                          (i/not= (nth digits i) (nth digits j)))
                        (apply i/conjunction))
          constraint (i/and
                      all-diff
                      (i/= (i/+ send more) money)
                      (i/> s 0)   ;; leading digits nonzero
                      (i/> m 0))
          solution (i/satisfy constraint)]
      ;; Verify the arithmetic identity holds
      (let [s* (get solution s) e* (get solution e) n* (get solution n) d* (get solution d)
            m* (get solution m) o* (get solution o) r* (get solution r) y* (get solution y)
            send* (+ (* s* 1000) (* e* 100) (* n* 10) d*)
            more* (+ (* m* 1000) (* o* 100) (* r* 10) e*)
            money* (+ (* m* 10000) (* o* 1000) (* n* 100) (* e* 10) y*)]
        (is (= (+ send* more*) money*))
        (is (pos? s*))
        (is (pos? m*))
        (is (= 8 (count (distinct [s* e* n* d* m* o* r* y*]))))))))

(deftest magic-square-3x3-test
  (testing "3x3 magic square"
    (let [cell-domain (range 1 10)
          cells (vec (repeatedly 9 #(i/fresh-int cell-domain)))
          [a b c d e f g h k] cells
          magic-sum (i/fresh-int (range 1 46) "magic")
          ;; all different
          all-diff (->> (for [i (range 9)
                              j (range (inc i) 9)]
                          (i/not= (nth cells i) (nth cells j)))
                        (apply i/conjunction))
          ;; rows, cols, diags all equal magic-sum
          sums (i/and
                (i/= magic-sum (i/+ a b c))
                (i/= magic-sum (i/+ d e f))
                (i/= magic-sum (i/+ g h k))
                (i/= magic-sum (i/+ a d g))
                (i/= magic-sum (i/+ b e h))
                (i/= magic-sum (i/+ c f k))
                (i/= magic-sum (i/+ a e k))
                (i/= magic-sum (i/+ c e g)))
          solution (i/satisfy (i/and all-diff sums))
          vals* (mapv solution cells)
          ms (get solution magic-sum)]
      ;; magic constant for 3x3 is always 15
      (is (= 15 ms))
      ;; all values distinct and in 1-9
      (is (= 9 (count (distinct vals*))))
      (is (every? #(<= 1 % 9) vals*))
      ;; verify row sums
      (is (= ms (apply + (subvec vals* 0 3))))
      (is (= ms (apply + (subvec vals* 3 6))))
      (is (= ms (apply + (subvec vals* 6 9)))))))

;; ============================================================
;; 2. Disjunction and or
;; ============================================================

(deftest disjunction-test
  (testing "or selects one of multiple alternatives"
    (let [x (i/fresh-int (range 50))]
      (is (contains?
           #{3 7}
           (get (i/satisfy (i/or (i/= x 3) (i/= x 7))) x)))))

  (testing "disjunction of many constraints"
    (let [x (i/fresh-int (range 50))
          constraint (apply i/disjunction
                            (map (fn [v] (i/= x v)) [10 20 30]))]
      (is (contains? #{10 20 30}
                     (get (i/satisfy constraint) x))))))

(deftest or-with-arithmetic-test
  (testing "or combined with arithmetic constraints"
    (let [x (i/fresh-int (range 50))
          y (i/fresh-int (range 50))
          solution (i/satisfy
                    (i/and
                     (i/or (i/= x 5) (i/= x 10))
                     (i/= y (i/* x 2))))]
      (is (= (* 2 (get solution x)) (get solution y)))
      (is (contains? #{5 10} (get solution x))))))

;; ============================================================
;; 3. Optimization (maximize)
;; ============================================================

(deftest maximize-basic-test
  (testing "maximize a bounded variable"
    (let [x (i/fresh-int (range 101))
          solution (i/maximize x (i/>= x 0))]
      (is (= 100 (get solution x))))))

(deftest maximize-with-constraints-test
  (testing "maximize subject to multiple constraints"
    (let [x (i/fresh-int (range 11))
          y (i/fresh-int (range 11))
          ;; maximize x+y where x,y in [0,10] and x+y <= 15
          constraint (i/<= (i/+ x y) 15)
          solution (i/maximize (i/+ x y) constraint)]
      (is (= 15 (+ (get solution x) (get solution y)))))))

(deftest maximize-knapsack-test
  (testing "simple 0-1 knapsack via maximize"
    ;; Items: weight/value pairs: (3,4), (4,5), (2,3), (5,7)
    ;; Capacity: 7
    (let [binary-domain (range 2)
          x0 (i/fresh-int binary-domain "x0") x1 (i/fresh-int binary-domain "x1")
          x2 (i/fresh-int binary-domain "x2") x3 (i/fresh-int binary-domain "x3")
          weight (i/+ (i/* 3 x0) (i/* 4 x1) (i/* 2 x2) (i/* 5 x3))
          value  (i/+ (i/* 4 x0) (i/* 5 x1) (i/* 3 x2) (i/* 7 x3))
          constraint (i/<= weight 7)
          solution (i/maximize value constraint)
          total-weight (+ (* 3 (get solution x0)) (* 4 (get solution x1))
                          (* 2 (get solution x2)) (* 5 (get solution x3)))
          total-value  (+ (* 4 (get solution x0)) (* 5 (get solution x1))
                          (* 3 (get solution x2)) (* 7 (get solution x3)))]
      (is (<= total-weight 7))
      ;; Optimal: x2=1, x3=1 -> w=7, v=10
      (is (= 10 total-value)))))

;; ============================================================
;; 4. N-Queens (classic CSP)
;; ============================================================

(deftest four-queens-test
  (testing "4-queens: place 4 non-attacking queens on 4x4 board"
    (let [n 4
          col-domain (range n)
          ;; queens[i] = column of queen in row i
          queens (vec (repeatedly n #(i/fresh-int col-domain)))
          ;; all columns different
          col-diff (->> (for [i (range n)
                              j (range (inc i) n)]
                          (i/not= (nth queens i) (nth queens j)))
                        (apply i/conjunction))
          ;; no two queens on same diagonal
          diag-diff (->> (for [i (range n)
                               j (range (inc i) n)
                               :let [di (- j i)]]
                           (i/and
                            (i/not= (i/+ (nth queens i) di) (nth queens j))
                            (i/not= (i/- (nth queens i) di) (nth queens j))))
                         (apply i/conjunction))
          solution (i/satisfy (i/and col-diff diag-diff))
          cols (mapv solution queens)]
      ;; verify: all columns distinct
      (is (= n (count (distinct cols))))
      ;; verify: no diagonal attacks
      (is (every? true?
                  (for [i (range n) j (range (inc i) n)]
                    (not= (Math/abs (- (nth cols i) (nth cols j)))
                           (- j i))))))))

;; ============================================================
;; 5. Set constraint problems
;; ============================================================

(deftest set-partition-test
  (testing "partition a set into two disjoint subsets with equal cardinality"
    (let [domain (range 6)
          a (i/fresh-set domain)
          b (i/fresh-set domain)
          constraint (i/and
                      (i/= (i/union a b) (set domain))
                      (i/= (i/count (i/intersection a b)) 0)
                      (i/= (i/count a) 3)
                      (i/= (i/count b) 3))
          solution (i/satisfy constraint)
          a* (get solution a)
          b* (get solution b)]
      (is (= (set domain) (clojure.set/union a* b*)))
      (is (empty? (clojure.set/intersection a* b*)))
      (is (= 3 (count a*)))
      (is (= 3 (count b*))))))

(deftest set-covering-test
  (testing "find minimal subset containing required elements"
    (let [domain (range 10)
          s (i/fresh-set domain)
          ;; must contain 1, 3, 5, 7
          constraint (i/and
                      (i/contains? s 1)
                      (i/contains? s 3)
                      (i/contains? s 5)
                      (i/contains? s 7)
                      (i/= (i/count s) 4))
          solution (i/satisfy constraint)
          s* (get solution s)]
      (is (= 4 (count s*)))
      (is (every? s* [1 3 5 7])))))

(deftest set-subset-chain-test
  (testing "chain of subsets: a subset b subset c"
    (let [domain (range 8)
          a (i/fresh-set domain)
          b (i/fresh-set domain)
          c (i/fresh-set domain)
          constraint (i/and
                      (i/= (i/count a) 2)
                      (i/= (i/count b) 4)
                      (i/= (i/count c) 6)
                      (i/subset? a b)
                      (i/subset? b c))
          solution (i/satisfy constraint)
          a* (get solution a) b* (get solution b) c* (get solution c)]
      (is (= 2 (count a*)))
      (is (= 4 (count b*)))
      (is (= 6 (count c*)))
      (is (clojure.set/subset? a* b*))
      (is (clojure.set/subset? b* c*)))))

;; ============================================================
;; 6. Quantifier-heavy problems
;; ============================================================

(deftest forall-all-elements-divisible-test
  (testing "forall: every element in set divisible by 3"
    (let [s (i/fresh-set (range 30))
          constraint (i/and
                      (i/= (i/count s) 4)
                      (i/forall (i/bind (range 30) s)
                        (fn [elem]
                          (i/= 0 (i/mod elem 3)))))
          solution (i/satisfy constraint)
          s* (get solution s)]
      (is (= 4 (count s*)))
      (is (every? #(zero? (mod % 3)) s*)))))

(deftest for-set-image-test
  (testing "for-set: compute image of a function over a set"
    (let [s (i/fresh-set (range 10))
          ;; image = {x+1 : x in s}
          image (i/for-set (i/bind (range 10) s)
                  (fn [x] (i/+ x 1)))
          constraint (i/and
                      (i/= s #{2 4 6})
                      (i/= #{3 5 7} image))
          solution (i/satisfy constraint)]
      (is (= #{2 4 6} (get solution s))))))

;; ============================================================
;; 7. Multi-variable arithmetic constraint propagation
;; ============================================================

(deftest simultaneous-equations-test
  (testing "system of linear equations"
    ;; x + y = 10, x - y = 2  => x=6, y=4
    (let [x (i/fresh-int (range 101))
          y (i/fresh-int (range 101))
          solution (i/satisfy
                    (i/and
                     (i/= (i/+ x y) 10)
                     (i/= (i/- x y) 2)))]
      (is (= 6 (get solution x)))
      (is (= 4 (get solution y))))))

(deftest transitive-equality-test
  (testing "equality chains propagate: a=b, b=c => a=c"
    (let [a (i/fresh-int (range 101))
          b (i/fresh-int (range 101))
          c (i/fresh-int (range 101))
          solution (i/satisfy
                    (i/and
                     (i/= a 42)
                     (i/= a b)
                     (i/= b c)))]
      (is (= 42 (get solution a)))
      (is (= 42 (get solution b)))
      (is (= 42 (get solution c))))))

(deftest abs-value-via-cond-test
  (testing "absolute value expressed via cond"
    (let [x (i/fresh-int (range -100 101))
          abs-x (i/fresh-int (range 101))
          solution (i/satisfy
                    (i/and
                     (i/= x -7)
                     (i/= abs-x (i/cond
                                  (i/>= x 0) x
                                  :else (i/- 0 x)))))]
      (is (= -7 (get solution x)))
      (is (= 7 (get solution abs-x))))))

;; ============================================================
;; 8. Bounded search with multiple constraints
;; ============================================================

(deftest pythagorean-triple-test
  (testing "find a Pythagorean triple a^2 + b^2 = c^2"
    (let [a (i/fresh-int (range 1 21))
          b (i/fresh-int (range 1 21))
          c (i/fresh-int (range 1 31))
          constraint (i/and
                      (i/>= b a)     ;; b >= a to avoid duplicates
                      (i/>= c b)
                      (i/= (i/+ (i/* a a) (i/* b b)) (i/* c c)))
          solution (i/satisfy constraint)
          a* (get solution a) b* (get solution b) c* (get solution c)]
      (is (= (+ (* a* a*) (* b* b*)) (* c* c*)))
      (is (<= 1 a* b* c*)))))

(deftest sum-of-consecutive-test
  (testing "find n consecutive integers that sum to target"
    ;; n consecutive starting from x: x + (x+1) + (x+2) = 12 => x=3
    (let [x (i/fresh-int (range 100))
          constraint (i/= (i/+ x (i/+ x 1) (i/+ x 2)) 12)]
      (is (= 3 (only-val (i/satisfy constraint)))))))

;; ============================================================
;; 9. Boolean constraint combinations
;; ============================================================

(deftest boolean-circuit-test
  (testing "boolean satisfiability: (a OR b) AND (NOT a OR c) AND (NOT b OR NOT c)"
    (let [binary (range 2)
          a (i/fresh-int binary) b (i/fresh-int binary) c (i/fresh-int binary)
          constraint (i/and
                      (i/or (i/= a 1) (i/= b 1))
                      (i/or (i/= a 0) (i/= c 1))
                      (i/or (i/= b 0) (i/= c 0)))
          solution (i/satisfy constraint)
          a* (get solution a) b* (get solution b) c* (get solution c)]
      ;; verify all clauses
      (is (or (= 1 a*) (= 1 b*)))
      (is (or (= 0 a*) (= 1 c*)))
      (is (or (= 0 b*) (= 0 c*))))))

;; ============================================================
;; 10. Music-theory flavored constraints
;; ============================================================

(deftest interval-class-vector-test
  (testing "find a pitch class set with distinct pairwise intervals"
    ;; Find 3 notes from 0-11 where all pairwise intervals are different
    ;; (an all-interval trichord). Use integer variables instead of sets
    ;; to avoid deeply nested forall.
    (let [pc-domain (range 12)
          a (i/fresh-int pc-domain "pc0")
          b (i/fresh-int pc-domain "pc1")
          c (i/fresh-int pc-domain "pc2")
          constraint (i/and
                      ;; ordered to avoid symmetry
                      (i/< a b) (i/< b c)
                      ;; all 3 pairwise intervals must be distinct
                      (i/not= (i/- b a) (i/- c b))
                      (i/not= (i/- b a) (i/- c a))
                      (i/not= (i/- c b) (i/- c a)))
          solution (i/satisfy constraint)
          a* (get solution a) b* (get solution b) c* (get solution c)
          intervals [(- b* a*) (- c* b*) (- c* a*)]]
      (is (< a* b* c*))
      (is (every? #(<= 0 % 11) [a* b* c*]))
      ;; all pairwise intervals distinct
      (is (= 3 (count (distinct intervals)))))))

(deftest voice-leading-smoothness-test
  (testing "minimize total voice movement between two chords"
    ;; Given chord 1 = [0, 4, 7] (C major)
    ;; Find assignment of C minor {0, 3, 7} to 3 voices minimizing total movement.
    ;; Use auxiliary variables for absolute differences to keep MiniZinc simple.
    (let [voice-domain (range 12)
          v1 (i/fresh-int voice-domain "v1")
          v2 (i/fresh-int voice-domain "v2")
          v3 (i/fresh-int voice-domain "v3")
          d1 (i/fresh-int (range 12) "d1")
          d2 (i/fresh-int (range 12) "d2")
          d3 (i/fresh-int (range 12) "d3")
          constraint (i/and
                      ;; chord 2 is C minor: {0, 3, 7}
                      (i/or (i/= v1 0) (i/= v1 3) (i/= v1 7))
                      (i/or (i/= v2 0) (i/= v2 3) (i/= v2 7))
                      (i/or (i/= v3 0) (i/= v3 3) (i/= v3 7))
                      ;; all different
                      (i/not= v1 v2)
                      (i/not= v1 v3)
                      (i/not= v2 v3)
                      ;; |v1 - 0| = d1
                      (i/>= d1 v1) (i/>= d1 (i/- 0 v1))
                      (i/or (i/= d1 v1) (i/= d1 (i/- 0 v1)))
                      ;; |v2 - 4| = d2
                      (i/>= d2 (i/- v2 4)) (i/>= d2 (i/- 4 v2))
                      (i/or (i/= d2 (i/- v2 4)) (i/= d2 (i/- 4 v2)))
                      ;; |v3 - 7| = d3
                      (i/>= d3 (i/- v3 7)) (i/>= d3 (i/- 7 v3))
                      (i/or (i/= d3 (i/- v3 7)) (i/= d3 (i/- 7 v3))))
          ;; minimize total = maximize negative total
          neg-total (i/- 0 (i/+ d1 d2 d3))
          solution (i/maximize neg-total constraint)
          v1* (get solution v1) v2* (get solution v2) v3* (get solution v3)
          total-move (+ (Math/abs (- v1* 0))
                        (Math/abs (- v2* 4))
                        (Math/abs (- v3* 7)))]
      ;; Optimal: v1=0, v2=3, v3=7 -> movement = 0+1+0 = 1
      (is (= #{0 3 7} #{v1* v2* v3*}))
      (is (= 1 total-move)))))
