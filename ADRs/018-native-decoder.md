# ADR 018 — A native, accumulating decoder (supersedes 006 and 012)
Status: Accepted | Date: 2026-10-22

## Context
0.1 converted the merged tree to an aeson `Value` (dropping provenance),
used `fromJSON` (dropping aeson's error path), then reported the
alphabetically first leaf as the error location. The error pointed at
the wrong key.

## Decision
`FromConfig` decodes figment's own `Value` tree in a `DecodeContext`
(path, profile, lossy flag, consulted sources). `Decoder` is an
**accumulating applicative**; it deliberately has no `Monad` instance,
because a monad has to short-circuit. `andThen` provides explicit
sequencing. Generic deriving covers records, enums, externally tagged
sums, newtypes and positional products, with `Options`
(`fieldLabelModifier`, `constructorTagModifier`,
`rejectUnknownFields` with edit-distance suggestions). Strict by default;
`extractLossy` opts in to string-to-number/bool coercion. Values parsed
from untyped text keep their original text (`tagRaw`), so `Text` fields
receive `007` verbatim. `ViaAeson` bridges existing `FromJSON` instances
and translates aeson's `JSONPath` into figment paths.

## Consequences
Every error carries the exact path, source, profile, line/column and
the provider-native name. Users with only `FromJSON` instances use
`deriving FromConfig via ViaAeson T`.
