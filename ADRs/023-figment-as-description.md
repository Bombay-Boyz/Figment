# ADR 023 — A Figment is a pure description; load collects everything
Status: Accepted | Date: 2026-10-22

## Decision
- `Figment` is a sequence of steps (order, metadata, provider action)
  plus the last `select` and `withLimits`. It is a lawful `Monoid` (the
  free monoid on steps), which is the lawful structure the value-level
  merge cannot provide. The laws are property-tested observationally.
- Builders (`merge`, `join`, `adjoin`, `admerge`, `combine`) take
  `HasCallStack` and record where each provider was added.
- `load` runs providers in order and **does not stop at the first
  failure**: all provider errors are returned together. Any synchronous
  exception that escapes a provider becomes an `IoError` with that
  provider's metadata; asynchronous exceptions are re-thrown.
- `Figment` is itself a `Provider`. Inner values keep their tags,
  because retagging only fills in missing provenance.
- Profiles: `Default`, then the selected profile, then `Global`. An
  explicit `select` beats profile hints from providers, which combine by
  the step's order.
