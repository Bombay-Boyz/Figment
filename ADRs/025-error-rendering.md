# ADR 025 — Error values and their rendering
Status: Accepted | Date: 2026-10-22

## Decision
`ConfigError { kind, path, profile, metadata, pos, hints }` with a
closed `ErrorKind`, every constructor of which is produced somewhere.
`ConfigErrors` is non-empty, a `Semigroup`, and an `Exception` whose
`displayException` is `renderErrors`. The rendered form is part of the
public interface and is pinned by golden tests in `test/golden/`.
(0.1 pinned derived `Show` output, which leaked the internal
`UnsafeDepth` constructor.)
