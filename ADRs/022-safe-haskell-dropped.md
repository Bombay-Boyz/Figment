# ADR 022 — Safe Haskell annotations removed (supersedes 014)
Status: Accepted | Date: 2026-10-22

## Context
In 0.1 the `Safe`/`Trustworthy` assignments contradicted ADR 014, and
`Trustworthy` was used to get past aeson's inferred status rather than
as the result of an audit. That is a promise nobody had checked.
Trustworthy guarantees also mean nothing unless downstream users enable
`-fpackage-trust`, which almost no one does. Every adapter now depends
on aeson, libyaml or toml-parser, none of which is Safe.

## Decision
No Safe Haskell pragmas. Purity is visible in the types: only `provide`,
`load` and the jail live in `IO`. The single `unsafePerformIO` (the
jail's process-wide lock, the standard top-level `MVar` idiom) is
documented where it appears.
