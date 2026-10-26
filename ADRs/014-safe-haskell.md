# ADR 014 — Safe Haskell boundary: Safe core, Trustworthy adapters

> **Superseded by ADR 022 (0.2.0.0): Safe Haskell annotations removed.**

Status: Accepted | Date: 2026-10-21 | Rule: §2.8, §1.7

## Context
Figment reads untrusted config input. Safe Haskell offers a
machine-checkable guarantee that a module contains no unsafePerformIO
and cannot violate type safety or referential transparency. The
question is which modules to mark, and with which of the three flags
(Safe, Trustworthy, Unsafe).

## Decision
- Pure core: `Figment.Prim`, `Figment.Value`, `Figment.Merge`,
  `Figment.Layer`, `Figment.Error`, `Figment.Extract`, `Figment.Prov`
  carry `{-# LANGUAGE Safe #-}`.
- IO adapters and façade: `Figment.Provider.*`, `Figment`,
  `Figment.Provider` carry `{-# LANGUAGE Trustworthy #-}`.
- A new test module `test/Figment/SafeBoundary.hs` opens with
  `{-# LANGUAGE Safe #-}` and imports only the pure-core modules,
  proving the machine-checked claim at test time.

## Rationale
- All-Safe is rejected: the provider adapters import aeson, tomly-parser,
  yaml, and env libraries whose Safe Haskell status is not under our
  control and varies by GHC version. A Safe module with one untrusted
  import fails to compile [GHC User's Guide §6.18].
- All-Trustworthy is rejected: it discards the machine-checked
  guarantee on the merge algebra and the depth-bound tagger, which are
  exactly the parts the feasibility study proved.
- Safe core + Trustworthy edge is the standard Haskell library pattern
  for "pure logic with a thin IO shell".

## Consequences
- Every pure-core module gains one pragma.
- If a pure-core module's imports become unsafe in a future GHC/package
  version, the build fails loudly at that import, not silently.
- `test/Figment/SafeBoundary.hs` is the mechanical check (Standard §2.8).
- The README trust-boundary section cites this ADR.
