# Changelog

## 0.2.0.0 — 2026-10-22

A breaking redesign, following a forensic audit of 0.1
(`docs/audit-remediation.md` maps every finding to its fix and test).

### Fixed
- Environment and CLI providers dropped every value (`APP_HOST=x` became
  `{host: {}}`).
- Decode errors reported the alphabetically first key instead of the
  failing one.
- The compile-time "provider check" was disconnected from the layers it
  claimed to check.
- Missing JSON/YAML files threw `IOException`; TOML caught asynchronous
  exceptions.
- TOML dates became `null`, `inf`/`nan` became garbage numbers, invalid
  UTF-8 was silently replaced; duplicate YAML keys and env conflicts
  were resolved silently.
- The depth limit ran after parsing, so it never protected the parsers.
- The README example did not compile.

### Added
- `Figment.Tutorial`: a sixteen-section guided tour whose snippets are
  compiled and run by the test suite.
- Profiles (`Default`, `Global`, custom; `select`; nested files; profile
  hints) with Rust figment's semantics.
- A native, accumulating decoder (`FromConfig`, generic deriving with
  options, combinators): every error is reported at once with path,
  source, line/column, profile and the provider-native key name.
- Higher-kinded partial configurations (`Field`, `Partial`,
  `extractPartial`, `extractTotal`).
- `Tagged`, `RelativePath`, `ViaAeson`; `extractLossy`, `extractAt`,
  `findValue`, `contains`, `findMetadata`, `focus`, `reselect`.
- Providers: `fromText`, `file` (optional, searched upward), `fileExact`,
  `required`, `nested`; Rust-compatible `Env` with options; revised
  `Cli`; `Serialized` (`defaults`, `globals`, `keyed`); figments as
  providers.
- Metadata: structured source, the call site where each provider was
  added, and a key-name interpolater.
- Parser limits (`Limits`: depth and node count) enforced inside every
  parser; YAML alias-bomb protection.
- `Figment.Jail` for configuration tests.
- `Exception` instance and a human-readable renderer for `ConfigErrors`.

### Removed
- `composeChecked`, `composeUnchecked`, `Figment.Check`, `RawJSON`,
  `mkTagged`, `ProvMap`/`runExtractProv` (superseded), `SourceId`,
  `Depth` (now `Limits`), Safe Haskell pragmas, the `yaml` dependency.

### Changed
- `base >= 4.20` (GHC 9.10 and 9.12, as tested).

## 0.1.0.0
Initial pre-release.
