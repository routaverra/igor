(ns routaverra.igor.cache
  "Disk cache for solver results. Keyed on a canonical representation of the
   constraint tree where gensym-based Decision IDs are replaced with
   depth-first encounter indices, making the key stable across REPL
   re-evaluations and JVM restarts.

   The constraint half of the key is run through `normalize` first so that
   compositions equivalent under the lattice laws (commutativity, idempotence,
   absorption, identity, annihilation) hash to the same entry.

   The cache stores a *set* of known solutions per canonical model, plus a
   :complete? flag indicating whether the solver proved completion (all
   solutions found / optimality proved). Every solve call adds to this set
   (union semantics), so partial results from timeouts accumulate across
   invocations."
  (:require [routaverra.igor.canonical :as canonical]
            [routaverra.igor.normalize :as normalize]
            [clojure.java.io :as io]))

(def ^:dynamic *enabled* true)
(def ^:dynamic *cache-dir* ".igor/cache")

;; Re-exported for backwards-compat with callers (and tests) that
;; accessed canonicalization through the cache namespace.
(def encounter-ordered-decisions canonical/encounter-ordered-decisions)
(def canonical-form canonical/canonical-form)

(defn cache-key
  "Returns a hex string hash for a canonical model.
   The key captures the constraint structure, objective, and solve direction,
   but NOT all? or async? — those are execution modes, not model identity.

   The constraint is run through `normalize` before canonicalization so that
   compositions equivalent under the lattice laws hash to the same entry.
   The objective is intentionally not normalized."
  [constraint objective opts]
  (let [normalized (normalize/normalize constraint)
        form (if objective
               (canonical/canonical-form normalized objective)
               (canonical/canonical-form normalized))
        key-data [form (:direction opts)]]
    (format "%016x" (hash key-data))))

(defn- cache-file ^java.io.File [key]
  (io/file *cache-dir* (str key ".edn")))

(defn clear!
  "Delete all cached solutions. Safe to call at any time."
  []
  (let [dir (io/file *cache-dir*)]
    (when (.exists dir)
      (doseq [f (.listFiles dir)]
        (.delete f)))))

(defn cache-read
  "Returns cached entry {:solutions #{...} :complete? bool} or nil."
  [key]
  (let [f (cache-file key)]
    (when (.exists f)
      (read-string (slurp f)))))

(defn cache-write [key entry]
  (let [f (cache-file key)]
    (io/make-parents f)
    (spit f (pr-str entry))))

(defn decisions-for [constraint objective]
  (apply encounter-ordered-decisions
         (if objective [constraint objective] [constraint])))

(defn solution->values
  "Convert a solution map to a value-vector in encounter order."
  [decisions solution]
  (mapv #(get solution %) decisions))

(defn values->solution
  "Convert a value-vector to a solution map keyed by current decisions."
  [decisions values]
  (zipmap decisions values))

(defn lookup-solutions
  "Returns cached entry {:solutions #{...} :complete? bool}, or nil if empty/missing."
  [constraint objective opts]
  (when *enabled*
    (let [key (cache-key constraint objective opts)
          cached (cache-read key)]
      (when (and cached (seq (:solutions cached)))
        cached))))

(defn add-solutions!
  "Merge new value-vectors into the cached solution set.
   complete? is OR'd: once true, stays true.
   Returns the merged entry {:solutions #{...} :complete? bool}."
  [constraint objective opts new-value-vectors complete?]
  (if *enabled*
    (let [key (cache-key constraint objective opts)
          existing (or (cache-read key) {:solutions #{} :complete? false})
          merged {:solutions (into (:solutions existing) new-value-vectors)
                  :complete? (or (:complete? existing) complete?)}]
      (when (not= existing merged)
        (cache-write key merged))
      merged)
    {:solutions (set new-value-vectors) :complete? complete?}))
