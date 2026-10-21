# ADR 012 — runExtract provenance-preserving decode (supersedes ADR 006)
Status: Accepted | Date: 2026-10-21 | Rule: §2.1, §2.8, §11

## Context
ADR 006 chose two-pass decode: `Tagged → aeson Value → a` with
provenance only available for error reporting, not on the decoded
result. That left Gate 2(b) of the feasibility study unsatisfied:
"does leaf-level provenance survive the round-trip through
aeson's FromJSON?"

## Decision
Provide an *additional* decoder alongside `runExtract`:

    runExtractProv
      :: forall a. (Generic a, Provenanced a)
      => Tagged -> Either ConfigError (a, ProvMap)

where `ProvMap = Map Path Metadata` collects the metadata of every
leaf in the source tree, keyed by path. `a` is decoded exactly as in
`runExtract` — provenance is *not* threaded through aeson; it is
collected in a second walk of the same tree and returned alongside.

Callers who need per-field provenance look up `Path` in the map. The
`Provenanced a` class supplies the path-aware field walk for `a`.
A default blanket instance covers every `Generic a` by walking `Rep a`.

## Rationale
Options (b) and (c) of ADR 006 both require threading metadata through
the parse. Both are invasive: (b) is a new class replacing FromJSON for
every field type; (c) needs parallel mirror types. The ProvMap approach
gives the same consumer-visible guarantee (leaf → provenance, reachable
by path) without changing how `a` is decoded, so it stays compatible
with every existing FromJSON instance.

## Consequences
- `runExtract` retains its existing behaviour and signature.
- `runExtractProv` is new; no existing call site changes.
- Gate 2(b) is satisfied at the `Path → Metadata` map level. Consumers
  that need provenance attached per-structure-field build a lookup on
  top; the primitive guarantee is the map.
