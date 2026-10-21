# ADR 003 — Manual NFData instances on the value domain
Status: Accepted | Date: 2026-10-21 | Rule: §5.6

## Context
criterion's `nf` requires NFData on the benchmarked output type.
`mkTagged` returns `Either ConfigError Tagged`; `mergeAlg` returns `Tagged`.

## Decision
Hand-written NFData instances on Key, SourceId, Path, Profile,
ProfileName, Depth, Value, Metadata, Tagged, ConfigError.

## Rationale
Deriving NFData via Generic failed on GHC 9.14. Manual instances work
identically on GHC 9.4.7 through 9.14 with no version-specific code.
`nf` on a projection (`nf (const () …)`) would benchmark allocation,
not the result (§2.8).

## Consequences
`deepseq` added to library build-depends. No runtime semantics change.
