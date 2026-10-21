# ADR 004 — cabal.project.freeze deferred to Phase 2
Status: Accepted (provisional) | Date: 2026-10-21 | Rule: §2.12

## Context
§2.12 requires a lockfile. cabal.project.freeze is per-GHC; a single
freeze file does not work across a five-version CI matrix.

## Decision
Phase 1 pins Hackage index date via index-state (in cabal.project).
The full per-GHC freeze matrix is deferred to Phase 2 via haskell-ci.

## Rationale
index-state is a necessary first step. Per-GHC freeze files are the
correct long-term answer and are produced/maintained by haskell-ci.

## Consequences
Known deviation until Phase 2. Not silent (§5.9).
