# ADR 006 — runExtract design: two-pass decode with provenance reattachment

Status: Proposed | Date: 2026-10-21 | Rule: §2.1, §2.8, §11

## Context
`runExtract :: FromJSON a => Tagged -> Either ConfigError a` is the
boundary from the merged tree back to a caller's record type. The
feasibility study §11 calls this Risk #1 and states that no bridge
is designed.

The load-bearing question (Gate 2(b)): does leaf-level provenance
(specifically `path`) survive the round-trip through aeson's FromJSON?

## Options considered

(a) Two-pass decode:
    1. Convert Tagged -> aeson Value, dropping metadata.
    2. Decode Value -> a with aeson's standard FromJSON.
    3. On DecodeError, re-walk the original Tagged tree to locate the
       failing path for the error message.
    Provenance is NOT threaded through decode; it is only available to
    error reporting, not to the decoded result.

(b) Custom FromJSON Tagged:
    Write a `FromJSON Tagged` instance that carries metadata through
    every Parser step. This preserves per-field provenance in the
    decoded result, but requires `a` to be built from Tagged, not from
    aeson Value — a new class, not FromJSON.

(c) Field-path reattachment post-decode:
    Decode to `a` via (a), then re-walk Tagged and `a` in parallel to
    reattach provenance to each leaf of `a`. Requires `a` to be
    reflectable via `Generic`, and requires a parallel data type that
    mirrors `a` with per-leaf Tagged values.

## Decision (proposed)
Option (a) for Phase 3. Rationale:
- Matches §2.1 (one canonical IR: Tagged is the IR, aeson Value is a
  wire form used only at the provider/decode edge).
- Option (b) is a new typeclass the study does not name; it is a much
  larger design than an ADR can commit to.
- Option (c) is Gate 2(b) work, not Phase 3 work — provenance
  reattachment requires Generic walkers of `a` that we do not have.
- Option (a) satisfies the §8.1 DecodeError contract: on failure, the
  error carries the failing Path, even if the successful result does
  not carry provenance per-leaf.

## Consequences
- Gate 2(b) is NOT satisfied by Phase 3. If provenance must survive
  decode, a future ADR chooses between (b) and (c).
- `DecodeError Path Text` gets its Path by a Tagged-tree walk on the
  failing field, not from aeson's internal parser state.
- `Figment.hs` exports `composeChecked` / `composeUnchecked` as
  specified in §8.2, both delegating to `runLayers` + `runExtract`.
