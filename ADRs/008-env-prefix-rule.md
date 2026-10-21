# ADR 008 — Env-var key decomposition rule
Status: Accepted | Date: 2026-10-21 | Rule: §2.8, §5.7 (RT-03)

## Context
The feasibility study §10 names RT-03 (env-var prefix collision) as a
regression class that must be pre-empted with a named test before the
Env provider is implemented.

## Decision
Given a prefix P and an env var name N:
1. If N does not start with P, ignore it.
2. Strip P.
3. Lowercase the remainder.
4. Split on '_'.
5. Drop empty components.
6. If the result is empty (N == P), ignore it.
7. Otherwise the resulting list is the env var's path.

Collisions at the same path (two vars mapping to the same path after
case folding) are resolved by last-wins in Map insertion order.

## Rationale
Underscore is the only portable separator across POSIX and Windows
environments. Case folding is required because some shells (Windows
notably) are case-insensitive; folding to lower-case is deterministic
and testable.

## Consequences
- Test/Figment/EnvProvider.hs pins each rule with a named case.
- Any future change to the rule requires superseding this ADR (§5.9).
