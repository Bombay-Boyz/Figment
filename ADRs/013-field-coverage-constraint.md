# ADR 013 — Field-level coverage is a runtime default-value concern, not compile-time

> **Superseded by ADR 021 (0.2.0.0): higher-kinded partial configurations provide a field-level guarantee after all, by construction rather than by inspecting providers.**

Status: Accepted (supersedes earlier draft) | Date: 2026-10-21 | Rule: §2.8, §6.1, §8.4

## Context
§6.1 of the feasibility study says the reachability check is over the
provider list, not individual fields, and Gate 2(d) asks whether a
per-field check is worth adding.

The Rust `figment` crate — the study's closest prior art — does not do
a field-level coverage check at all. It lets the configuration struct
declare defaults via `#[serde(default)]`. Missing required fields
surface at extraction time; missing optional/defaulted fields fall back
to their declared default. Serde is the single source of truth.

## Decision
Do NOT add a field-level type-level coverage constraint. Keep
`RequireGenericProvider` as the only compile-time reachability check.
Field coverage is a responsibility of the `FromJSON a` instance
together with whatever default mechanism the caller chooses — `Maybe`
fields, `Default` + aeson's `allowOmittedFields`/`omittedField`
(aeson ≥ 2.2), or explicit defaulting logic.

Ensure `runExtract` returns a structured `MissingKey Path` when a
required field has no supplied value (already true at the type level;
field-level path reporting is a separate aeson-interop concern).

## Rationale
- A file provider reads user input; the compiler cannot know which
  keys it will contain. Any type-level check is either equivalent to
  the existing `RequireGenericProvider` (worthless) or requires the
  caller to declare per-field coverage (heavyweight, surprising, and
  unlike any comparable library).
- The default-value mechanism is already idiomatic in Haskell via
  aeson. Reimplementing it as a compile-time reachability check
  duplicates the responsibility in a less capable place.
- Aligning with Rust `figment` matters for interoperability and for
  users bringing mental models from that ecosystem.

## Consequences
- `FieldsOf` / `OptionalFields` (§8.4) remain exposed as building
  blocks for future use, but are not used by any constraint in this
  release.
- `composeChecked` gains no new constraints.
- README documents the required/defaulted-field discipline (see
  README section "Required and optional fields").
- test/CompileFail/ keeps `NoProvider` and `OkProvider`; drops the
  `RequiredAndMaybe` case (no longer meaningful).
