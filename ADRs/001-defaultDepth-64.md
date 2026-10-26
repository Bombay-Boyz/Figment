# ADR 001 — defaultDepth = 64

> **Amended by ADR 017 (0.2.0.0): the depth budget is now one of two `Limits`, enforced inside every parser; the root dictionary counts as depth 1.**


**Status:** Accepted (provisional; see Gate 2(e))
**Date:** 2026-10-21
**Standard rule under review:** §5.9 (deviations recorded)

## Context

`Figment.Value.defaultDepth` is the recursion budget every built-in
provider gets when the caller does not override it. The feasibility study
(§1.3, §11 item 3, §12 Gate 2(e)) records this value as "a judgment call,
not a proof".

## Decision

Use `64`.

## Rationale

- Any hand-authored configuration schema deeper than 64 levels is already
  a schema smell; 64 comfortably exceeds real configs while bounding
  worst-case stack use to a small constant (Standard §4.10).
- Callers whose schema legitimately needs more can pass an explicit
  `Depth` to `tagProvider`; the default is not a hard ceiling.

## What this ADR does *not* claim

- That 64 is empirically correct. It is provisional. Gate 2(e) (Standard
  §5.6 benchmark on a realistic config, feasibility study §12) is the
  evidence-gathering step, and if it shows a different number is right,
  this ADR is superseded rather than edited.

## Consequences

- `Figment.Value.defaultDepth` stays a single named constant.
- Any change to `64` is a breaking change only if it lowers the value
  (would turn currently-accepted inputs into `DepthExceeded`); raising it
  is non-breaking.
