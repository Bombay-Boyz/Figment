# Changelog

## 0.3.0.0 — unreleased

Hardening release. It follows 0.2.0.0 and contains everything merged after
the `v0.2.0.0` tag. It is a major version under the PVP because the public
`ErrorKind` type gained a constructor (`TooManyBytes`), which breaks
exhaustive pattern matches in user code.

### Hardening (source)

- Error rendering now escapes control, bidirectional and zero-width characters
  in every message, hint and type description, not just keys and file names.
- A leading byte-order mark is ignored in JSON and TOML input.
- TOML: the linear pre-scan also counts structural tokens and rejects a
  document with more than `5 * maxNodes` of them as `TooManyNodes`, before
  toml-parser builds its whole syntax tree.

### Added
- `Secret a`: a wrapper whose decode errors never quote the value and whose
  `Show` prints `Secret <redacted>`. Use it for passwords and tokens.
- `FromConfigKey` instances for `Int` and `Integer`, so `Map Int a` decodes.
- Environment and CLI values: structured values obey `maxBytes` and
  `maxNodes`; duplicate keys in an inline table are reported; `\uXXXX`
  escapes decode and unknown escapes are no longer silently dropped.
- Date-time decoders accept a space between date and time (RFC 3339).
- The CLI provider names options as mapped by a custom `keyMap`.
- `base` upper bound widened to `< 4.23` (GHC 9.14). Dependencies may not
  support GHC 9.14 yet; it is tested in CI as an experimental job only.
- `TooManyBytes` error kind and a per-source size limit: `maxBytes`,
  `withMaxBytes`, `defaultMaxBytes` (32 MiB by default). Files larger than
  the limit are never opened for reading; other sources are checked before
  parsing.
- A linear pre-scan that rejects deeply bracketed TOML before the parser
  builds it.
- Error text escaping: control characters, line and paragraph separators
  and bidirectional overrides in keys, file names and messages can no
  longer forge report lines; reports show at most 100 errors.
- Handle guards in `Figment.Jail`: nested jails, use after the jail ends
  and paths that escape the jail are errors instead of hangs or silent
  misbehaviour.
- `SECURITY.md`, `CONTRIBUTING.md`, issue and pull request templates,
  `.hlint.yaml`, and ADR 026 (the `unsafePerformIO` in `Figment.Jail`).

### Changed
- `load` now evaluates each provider's result inside its exception
  handler, so an exception hidden in a lazily built value becomes an
  `IoError` instead of escaping later.
- A YAML mapping may contain at most one merge key (`<<`); a second is a
  `DuplicateKey` error.
- YAML parsing gives up as soon as the event stream exceeds the node or
  depth limit instead of buffering all of it.
- Unreadable locations during the upward search for a file are reported
  instead of being treated as an absent file.
- Suggestions for unknown fields are computed with bounded work.
- `ENGINEERING_STANDARD.md` is no longer shipped in the source
  distribution (it remains in the repository).

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
