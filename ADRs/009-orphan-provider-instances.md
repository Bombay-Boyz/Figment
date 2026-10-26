# ADR 009 — Provider instances are orphans in their adapter modules

> **Superseded (0.2.0.0): each provider type is now defined in the same module as its `Provider` instance, so there are no orphan instances.**

Status: Accepted | Date: 2026-10-21 | Rule: §1.7, §2.6

## Context
The feasibility study §9 places each provider type in Figment.Provider
and its instance in Figment.Provider.<Format>. That creates an orphan
instance under GHC's rules: neither the class (Provider) nor the type
(EnvPrefixed) is defined in the module containing the instance.

## Decision
Keep the layout from §9 and silence -Worphans in each adapter module
that declares a Provider instance, with a one-line comment pointing
here.

## Rationale
Alternative (a) — put EnvPrefixed and its instance both in Env.hs —
    inverts the dependency direction: Figment.Check would have to import
    from an adapter to reach the type, violating §2.6.
Alternative (b) — put the instance in Figment.Provider — forces
    Figment.Provider to import System.Environment, violating §1.6
    (IO confined to adapters).
Alternative (c) — newtype-wrap in each adapter — adds a layer of
    boilerplate per provider for no semantic gain.

The Standard's §1.7 orphan ban is about *unrelated* modules; here the
"orphan" module is literally the adapter for that provider.

## Consequences
Each Provider/<Format>.hs opens with -Wno-orphans and this ADR is
referenced. Scope: adapter modules only.
