# ADR 015 — GHC 9.14.1 support deferred

> **Amended (0.2.0.0): tested-with is GHC 9.10.3 and 9.12.4 (the earlier 9.4.7 to 9.12.1 range was never actually tested). The stale `cabal.project.freeze.ghc-9.14.1` was removed. 9.14 remains untested.**

Status: Accepted | Date: 2026-10-22 | Rule: §2.11, §5.9

## Context
The library builds and passes all 57 tests on GHC 9.12.1. On GHC 9.14.1,
the solver cannot satisfy `hashable-1.5.0.0`, which requires `base <
4.22`; GHC 9.14.1 ships `base-4.22.0.0`. This is a Hackage upper bound,
not a bug in this codebase.

`allow-newer: hashable:base` would let 9.14.1 build, but would lie
about compatibility to downstream users.

## Decision
Ship with `tested-with: GHC 9.4.7 - 9.12.1`. Defer 9.14.1 until a
`hashable` release supports `base-4.22`.

## Consequences
- CI matrix stays at 9.4.7 - 9.12.1.
- 9.14.1 becomes a one-line `tested-with` change plus a freeze refresh
  when `hashable` publishes.
- Users on 9.14.1 install a library that does not officially support
  their compiler — the standard behaviour for any package whose
  transitive deps have not caught up.
