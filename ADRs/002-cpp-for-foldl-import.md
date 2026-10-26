# ADR 002 — CPP guard for the foldl' import in Figment.Layer

> **Superseded by ADR 016 (0.2.0.0): base >= 4.20 only, so `foldl'` comes from the Prelude and the codebase has no CPP.**

Status: Accepted | Date: 2026-10-21 | Rule: §1.8, §0.7

## Context
`foldl'` is in Data.List/Data.Foldable on base < 4.20 (GHC < 9.10);
it is re-exported by Prelude on base >= 4.20 (GHC >= 9.10). An
unconditional import fails -Wunused-imports under -Werror on GHC >= 9.10;
omitting it fails scope on GHC < 9.10.

## Decision
CPP-guarded import with CPP in default-extensions:

    #if !MIN_VERSION_base(4,20,0)
    import Data.Foldable (foldl')
    #endif

## Rationale
Dropping GHC < 9.10 rejected (matrix requirement). Reimplementing foldl'
violates §4.7. CPP is the standard zero-cost mechanism for this case.

## Consequences
This is the only CPP in the codebase and should remain so.
