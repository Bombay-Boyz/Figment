# ADR 005 — QuickCheck upper bound relaxed to < 2.20
Status: Accepted | Date: 2026-10-21 | Rule: §2.11

## Context
aeson-2.3.2.0 (pulled in by criterion) depends on QuickCheck >= 2.19.
The original bound `QuickCheck < 2.16` prevented the solver from
finding a plan that includes both test and bench components.

## Decision
Relax the bound to `QuickCheck >= 2.14.3 && < 2.20`.

## Rationale
The properties use only the stable QuickCheck API (forAll, ===,
counterexample, chooseInt, elements, frequency, vectorOf, once). Nothing
in the suite depends on behaviour that changed between 2.14 and 2.19.

## Consequences
Test-suite and benchmark components can now be solved together.
