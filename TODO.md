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
