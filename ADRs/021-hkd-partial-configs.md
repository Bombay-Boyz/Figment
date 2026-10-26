# ADR 021 — Higher-kinded partial configurations replace composeChecked
Status: Accepted | Date: 2026-10-22 | Supersedes: 007, 013

## Context
`composeChecked @R @ps` took a type-level provider list that was not
connected to the layers actually passed (its own `OkProvider` fixture
compiled with an empty list). Its premise was also weak: an env provider
being present says nothing about whether a field is supplied.

## Decision
Remove `composeChecked`/`composeUnchecked`. Offer `Field f a` (where
`Field Identity a = a`) and the `Partial` class, all of whose methods
are generic defaults:
- `extractPartial defaults` decodes whatever is present (all type
  errors), overlays it on partial defaults, and reports **every**
  missing field with the env var, flag or key that would set it;
- `extractTotal` takes total defaults (`c Identity`), so a missing-field
  error is impossible by the type.
This is a guarantee Rust's figment cannot express.

## Verification
`test/Figment/Test/Partial.hs`; `test/compile-fail/PartialSum.hs`
shows that sum types are rejected with a named type error.
