# TODO

## Higher-order constraint patterns

### Cumulative (resource scheduling)
The classic parallel-machine scheduling constraint. Each task has a start time, duration, and resource demand; total demand at any time point cannot exceed capacity. MiniZinc has native `cumulative` support.

Natural fit for the handle pattern: create task handles bundling start/duration/end variables, pass them to a `cumulative` constraint. Current scheduling tests (job sequencing, task assignment) work around this with manual cumulative sums, but that only handles single-machine / sequential problems.

## Type system

### Vector and list types
Add variable-length and fixed-length array types backed by MiniZinc arrays. Unlocks native support for: global cardinality (gcc), sort, increasing/decreasing, lexicographic ordering, and other array-based global constraints.

### ~~Keywords as enum type~~ (done)
Implemented. Keywords map to a single sorted MiniZinc enum. Supports `fresh-keyword`, keyword sets via `fresh-set`, `=`, `not=`, `contains?`, `all-different`. Both namespaced and non-namespaced keywords work.

### Sequence type (array/vec)
Native seq type that `all-different`, `regular`, `table`, and other array-accepting constraints can operate on directly. Currently these constraints take a vec of individual decision variables — there is no first-class "sequence of variables" type. A native seq type would:
- Allow `all-different` to accept a single seq variable instead of N individual vars
- Enable mixed-type seqs (e.g. a seq of keyword vars) without manual integer encoding
- Unblock keyword support for `regular` and `cost-regular` (currently numeric-only because the translation assumes integer arithmetic on alphabet elements; a seq type with typed elements could handle the enum→int casting internally)

Current workaround for `all-different` on keyword-typed values: encode as integers via a combined index (e.g. `onset_idx * n_vowels + vowel_idx`), apply `all-different` on the integer vars, decode back to keywords after solving. See `solve-verbs` in `platform/scores/ikaria/language.clj` for an example.

## Compositional / lattice work

### Native solver: incremental meet API
Lives in the `igor-native-solver` worktree. The native solver's store is already an immutable propagator engine — expose it directly so `meet` and `relax` become primitive operations on solver state:

```clojure
(def state  (native/init decisions bindings))
(def state' (native/meet state new-constraint))    ;; add propagator, re-run fixpoint
(def state'' (native/relax state' new-constraint)) ;; re-propagate from a saved checkpoint
(native/solutions state')                          ;; enumerate from current position
```

Persistent-map structural sharing makes `meet` cheap (~1ms function call) and backtracking free (snapshots are just references). This is the foundation for interactive composition UX where the user adds/removes constraints and watches domains shrink in real time.

The two-tier vision: native solver for the composition tier (low-latency incremental meet, no global constraints needed), MiniZinc for the production tier (Gecode/Chuffed/OR-Tools, full global constraint support). The existing `{:solver :native}` / `{:solver :minizinc}` selection mechanism already supports this — what's missing is the incremental API surface.

See `specs/constraint-lattice-programming.md` §2 for the full motivation.

### Compositional unit metadata (deferred)
Spec §3 sketches a metadata-based way to mark named compositional units (`(i/relax my-constraints in-range)` removing the bounded-x clause as a unit). Defer until a real workflow asks for named relaxation; in the meantime, users can hold their own structure via vars.

# Deferred type-system extensions (NOT for current release)

Governing principle: the public core is a CLOSED, TOTAL algebra over
homogeneous domains — every shadowed operator is defined on every value
in a variable's domain. Extensions below add partiality or structure;
each is introduced LATER as a deliberate, parametric layer over the
closed core, never by weakening the core. Order matters: Maybe first,
List second (it is built from Maybe). String constraints are rejected
for the foreseeable future (see bottom).

## Maybe τ  — absence as a type  [highest priority, do first]
Models "a value from domain D, or absent" (e.g. note-or-rest = Maybe Pitch).

- Type constructor over any base type: `(maybe (domain D))`.
- Distinct type from τ. Base operators (+, <, all-different, ...) are
  NOT defined on `Maybe τ`. You must ELIMINATE the Maybe first — via
  if / cond / alternatives / equality — into the `just` branch, where
  you are back in total-τ land and every operator works. This makes the
  "constraints read like Clojure" property hold by TYPING, not by
  remembering to guard.
- Likely encoding: ride on MiniZinc's native optional variables
  (`var opt`) — a presence-bool plus a value-var whose constraints are
  conditioned on presence. Confirm before committing to a hand-rolled
  presence-bool encoding.
- Lifting questions (single coherent question "how do globals lift over
  Maybe", not per-operator special cases):
  - `all-different` over `Maybe τ` ranges over the PRESENT (`just`)
    values only; absent slots never collide. (Music needs many rests.)
  - `regular` / `cost-regular` over a `Maybe` sequence is a FEATURE:
    a DFA over pitches-and-rests is rhythmic/phrase structure.
  - Notation rendering needs a glyph for absence.

### REJECTED alternative: absence-as-tag (flat sentinel domain)
Encoding absence as a sentinel member of a flat domain (e.g.
`int(0..11) | :rest`) is rejected. Because keywords encode to ints
underneath, `(i/+ x 2)` where x resolves to `:rest` would SILENTLY
compute on the sentinel's int code and return garbage as a "valid"
solution — the worst failure mode for this library. Absence must be a
type (Maybe), not a tag.

## List τ / variable-length sequences  [second; built on Maybe]
- Fixed-length homogeneous list already exists: `(vec (repeatedly n
  #(domain D)))`. A `list` constructor there is pure sugar (legibility,
  not power) — low priority.
- Variable-length (length is itself a decision variable) is the real
  feature, and it is `Maybe` wearing a prefix constraint: an array of
  `n` slots of `Maybe τ` + a length var + "present slots form a prefix
  (no gaps)". So it is DOWNSTREAM of Maybe — do not re-derive absence
  inside List.
- Performance footgun to document: the solver reasons about HOW MANY
  elements exist before WHAT they are. Ship with guidance to bound the
  max length tightly, as you would never leave an int domain unbounded.

## Constructor surface for the above
Prefer wrapper constructors — `(maybe (domain D))`, `(list-of (domain D)
{:max n})` — that read as type constructors and leave `domain` /
`universe` / `bool` untouched. The wrapper form IS the parametricity
made visible. Do not fold these into `domain`.

## String constraints  — REJECTED as in-scope; possible separate project
A `clojure.string`-style constraint library (`starts-with?`, etc.) is
NOT a library addition. Standard MiniZinc has no string decision
variables; string-constraint solving is its own research subfield
(cf. G-Strings/Gecode, the Amadini "MiniZinc with Strings" extension,
SMT string theories). The fixed-length case is largely already
reachable via `regular` / `table` / `nth` over char-as-int domains; the
powerful unbounded case is a research contribution, not a side feature.
Note: phonotactic/conlang use cases are regular languages — model them
with `regular` over a phoneme domain rather than a string engine. If a
genuine need survives that test, it is a future paper, not a patch.
