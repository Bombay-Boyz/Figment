# ADR 007 — Disable -Wredundant-constraints in Figment.hs

> **Superseded by ADR 021 (0.2.0.0): `composeChecked` was removed, so the warning suppression went with it.**

Status: Accepted | Date: 2026-10-21 | Rule: §5.1

## Context
`composeChecked`'s `RequireGenericProvider r ps` constraint is a
type-level-only compile-time gate. GHC sees no path where it is used
inside the body and reports it as redundant under
`-Wredundant-constraints`, which §5.1 makes an error.

## Decision
Add `{-# OPTIONS_GHC -Wno-redundant-constraints #-}` at the top of
`Figment.hs` only.

## Rationale
The constraint is not redundant — it is exactly what makes §6.2's
TypeError fire at call sites. GHC's redundancy analysis cannot see
type-level-only constraints as "used" because they discharge no
runtime evidence. Replacing the constraint with a class-with-method
would add a runtime witness no caller needs.

## Consequences
Scope: one module only. The rest of the library keeps the §5.1
warning. `composeChecked`'s call sites still get the §6.2 TypeError
when no generic provider is present (verified by
test/CompileFail/NoProvider.hs).
