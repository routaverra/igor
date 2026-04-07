(ns routaverra.igor.canonical
  "Decision-renaming canonicalization shared by `cache` and `normalize`.

   Replaces gensym Decision IDs with depth-first encounter indices so
   structurally-identical expressions hash to the same key regardless of
   how their decision variables were minted."
  (:require [routaverra.igor.api :as api]
            [clojure.walk :as walk]))

(defn encounter-ordered-decisions
  "Walks expression trees depth-first, returns decisions in order of
   first encounter. Each decision appears exactly once."
  [& exprs]
  (let [seen (volatile! #{})
        result (volatile! [])]
    (walk/prewalk
     (fn [node]
       (when (and (api/decision? node)
                  (not (contains? @seen (:id node))))
         (vswap! seen conj (:id node))
         (vswap! result conj node))
       node)
     (vec exprs))
    @result))

(defn canonical-form
  "Produces a canonical representation of constraint expressions where
   Decision variables are replaced with [::var idx domain type] vectors
   based on depth-first encounter order. Independent of gensym IDs."
  [& exprs]
  (let [id->idx (volatile! {})
        counter (volatile! -1)]
    (walk/prewalk
     (fn [node]
       (if (api/decision? node)
         (let [idx (or (get @id->idx (:id node))
                       (let [c (vswap! counter clojure.core/inc)]
                         (vswap! id->idx assoc (:id node) c)
                         c))]
           [::var idx
            (some-> (:routaverra.igor.api/range (meta node)) sort vec)
            (:routaverra.igor.api/type (meta node))])
         node))
     (vec exprs))))
