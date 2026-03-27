# TODO

## Higher-order constraint patterns

### Cumulative (resource scheduling)
The classic parallel-machine scheduling constraint. Each task has a start time, duration, and resource demand; total demand at any time point cannot exceed capacity. MiniZinc has native `cumulative` support.

Natural fit for the handle pattern: create task handles bundling start/duration/end variables, pass them to a `cumulative` constraint. Current scheduling tests (job sequencing, task assignment) work around this with manual cumulative sums, but that only handles single-machine / sequential problems.

### Soft constraints
"Prefer X, but allow violation at a cost." Wrap a constraint to get a handle with a satisfaction indicator and a cost variable tracking violation penalty. Minimize total cost across soft constraints.

API sketch: `(i/soft constraint penalty)` returns a handle; `(i/violation handle)` returns the cost variable. Composes naturally with `minimize`.

## Type system

### Vector and list types
Add variable-length and fixed-length array types backed by MiniZinc arrays. Unlocks native support for: global cardinality (gcc), sort, increasing/decreasing, lexicographic ordering, and other array-based global constraints.

### Keywords as enum type
Map Clojure keywords used on the Clojure side to a single MiniZinc enum (sorted to reflect sorted keywords). Keywords can then be used as domain values in `set(enum)`, `vec(enum)`, etc. This lets users write constraints over symbolic values directly rather than encoding everything as integers.
