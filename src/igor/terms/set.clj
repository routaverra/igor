(ns igor.terms.set
  (:require [igor.protocols :as protocols]
            [igor.types :as types]
            [igor.api :as api]
            [igor.set :as i.set]
            [clojure.set :as set]))

(defrecord TermIntersection [argv]
  protocols/IExpress
  (write [_self] (apply list 'clojure.set/intersection (map protocols/write argv)))
  (codomain [self] {types/Set self})
  (domainv [self] (repeat {types/Set self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "intersect" (map protocols/translate (:argv self)))))

(defrecord TermDifference [argv]
  protocols/IExpress
  (write [_self] (apply list 'clojure.set/difference (map protocols/write argv)))
  (codomain [self] {types/Set self})
  (domainv [self] (repeat {types/Set self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "diff" (map protocols/translate (:argv self)))))

(defrecord TermSymDiff [argv]
  protocols/IExpress
  (write [_self] (apply list 'igor.set/sym-diff (map protocols/write argv)))
  (codomain [self] {types/Set self})
  (domainv [self] (repeat {types/Set self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "symdiff" (map protocols/translate (:argv self)))))

(defrecord TermUnion [argv]
  protocols/IExpress
  (write [_self] (apply list 'clojure.set/union (map protocols/write argv)))
  (codomain [self] {types/Set self})
  (domainv [self] (repeat {types/Set self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "union" (map protocols/translate (:argv self)))))

(defrecord TermSubset [argv]
  protocols/IExpress
  (write [_self] (apply list 'clojure.set/subset? (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (repeat {types/Set self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "subset" (map protocols/translate (:argv self)))))

(defrecord TermSuperset [argv]
  protocols/IExpress
  (write [_self] (apply list 'clojure.set/superset? (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (repeat {types/Set self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (api/translate-nary-operation "superset" (map protocols/translate (:argv self)))))

(defrecord TermSetLt [argv]
  protocols/IExpress
  (write [_self] (apply list 'set-lt (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (repeat {types/Set self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (str "set_lt(" (protocols/translate (first (:argv self)))
                         ", " (protocols/translate (second (:argv self))) ")")))

(defrecord TermSetLe [argv]
  protocols/IExpress
  (write [_self] (apply list 'set-le (map protocols/write argv)))
  (codomain [self] {types/Bool self})
  (domainv [self] (repeat {types/Set self}))
  (decisions [self] (api/unify-argv-decisions self))
  (bindings [self] (api/unify-argv-bindings self))
  (validate [self] (api/validate-domains self))
  (translate [self] (str "set_le(" (protocols/translate (first (:argv self)))
                         ", " (protocols/translate (second (:argv self))) ")")))

;; --- Constructor functions ---

(defn- all-ground-sets? [args] (every? set? args))

(defn intersection [& args]
  (if (all-ground-sets? args)
    (apply set/intersection args)
    (api/cacheing-validate (->TermIntersection (vec args)))))
(defn difference [& args]
  (if (all-ground-sets? args)
    (apply set/difference args)
    (api/cacheing-validate (->TermDifference (vec args)))))
(defn sym-diff [& args]
  (if (all-ground-sets? args)
    (reduce i.set/sym-diff args)
    (api/cacheing-validate (->TermSymDiff (vec args)))))
(defn union [& args]
  (if (all-ground-sets? args)
    (apply set/union args)
    (api/cacheing-validate (->TermUnion (vec args)))))
(defn subset? [& args]
  (if (all-ground-sets? args)
    (every? true? (map (fn [[a b]] (set/subset? a b)) (partition 2 1 args)))
    (api/cacheing-validate (->TermSubset (vec args)))))
(defn superset? [& args]
  (if (all-ground-sets? args)
    (every? true? (map (fn [[a b]] (set/superset? a b)) (partition 2 1 args)))
    (api/cacheing-validate (->TermSuperset (vec args)))))

(defn- set-lex-compare
  "Lexicographic comparison on sorted element sequences.
   Returns neg/zero/pos like compare."
  [a b]
  (let [sa (sort a) sb (sort b)]
    (loop [[x & xs] sa [y & ys] sb]
      (cond
        (and (nil? x) (nil? y)) 0
        (nil? x) -1
        (nil? y)  1
        (< x y)  -1
        (> x y)   1
        :else (recur xs ys)))))

(defn set< [a b]
  (if (all-ground-sets? [a b])
    (neg? (set-lex-compare a b))
    (api/cacheing-validate (->TermSetLt [a b]))))

(defn set<= [a b]
  (if (all-ground-sets? [a b])
    (<= (set-lex-compare a b) 0)
    (api/cacheing-validate (->TermSetLe [a b]))))
