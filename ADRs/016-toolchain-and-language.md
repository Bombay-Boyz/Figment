# ADR 016 — Toolchain, language edition, dependencies
Status: Accepted | Date: 2026-10-22 | Supersedes: 002, 003

## Decision
- `default-language: GHC2021` plus `DerivingStrategies`, `DeriveAnyClass`,
  `LambdaCase`, `OverloadedStrings`. Every `deriving` clause names its
  strategy, so `DeriveAnyClass` can never be picked silently.
- `base >= 4.20 && < 4.22` (GHC 9.10, 9.12): the bounds claim exactly the
  compilers CI tests. No CPP anywhere.
- Instances are derived where possible (`NFData` via `Generic`).
- Dependencies: aeson (token stream, bridge), toml-parser (TOML with
  positions), libyaml + conduit (the YAML event stream), time (dates),
  directory/filepath/temporary (files, jail), containers, vector,
  scientific, text, bytestring, deepseq. `yaml` is no longer used, because
  its aeson-based decoder hides positions, duplicates and alias expansion.

## Consequences
Users on GHC < 9.10 must stay on 0.1. Widening the bounds is only a
`tested-with` change once CI covers the extra compilers.
