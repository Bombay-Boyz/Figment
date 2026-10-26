# Audit remediation (0.1.0.0 → 0.2.0.0)

Each finding from the forensic audit, what changed, and the test that
pins it. Test names are tasty patterns (`cabal test --test-options='-p "..."'`).

## Critical

| # | Finding | Resolution | Pinned by |
|---|---|---|---|
| C1 | `EnvPrefixed`/`CliArgs` stored `{}` for every value | Shared, tested `Internal.Nest`; values parsed and kept | `REGRESSION C1/H1` (env), `REGRESSION C1` (cli) |
| C2 | `composeChecked`'s type list was not tied to the layers | Removed. Replaced by HKD partial configs with a real guarantee (ADR 021) | `partial (HKD)`; `test/compile-fail/*` |
| C3 | Decode errors reported the first leaf's path | Native decoder; exact paths; all errors accumulate (ADR 018) | `REGRESSION C3`, `all errors are reported at once` |
| C4 | Profiles were dead code | Default → selected → global; `select`; nested files; hints (ADR 023) | `default profile, then selected, then global`, `nested files` |
| C5 | `readFile` threw; TOML caught async exceptions | `IOException` handled per provider; safety net in `load` re-throws async exceptions | `a missing required file`, `an exception escaping a provider becomes an error`, fuzz properties |
| C6 | Silent data loss: TOML dates→null, inf/nan garbage, UTF-8 replaced, duplicate keys | Dates are ISO strings decoding to `Data.Time`; non-finite numbers kept; strict UTF-8; duplicates are errors in JSON, YAML and TOML; env/CLI conflicts are errors | `dates and times`, `inf and nan`, `invalid UTF-8`, `duplicate keys`, `conflicts are reported` |

## High

| # | Finding | Resolution | Pinned by |
|---|---|---|---|
| H1 | Env/CLI values were all strings | TOML-like value parsing, original text kept in `tagRaw` (`007` stays `007` for `Text`) | `REGRESSION C1/H1`, `structured values` |
| H2 | Always split on `_`; underscored fields unreachable | No split by default; opt-in `split "__"` (ADR 019) | `REGRESSION H2` |
| H3 | README example did not compile | README examples are compiled and run by the test suite | `README` group |
| H4 | "No Semigroup" argument misplaced | `Figment` is a lawful `Monoid` (free monoid on steps); `Value` has none (non-associativity demonstrated) | `Figment is a monoid`, `not associative in general`, `test/compile-fail/NoValueSemigroup.hs` |
| H5 | Container tags lost; concat corrupted stored paths | Tags follow Rust (join keeps left, merge keeps right); paths are positional, never stored, so they cannot go stale | `container tags follow Rust`, `duality` |
| H6 | Depth guard ran after the parsers | `Limits` enforced inside every parser; alias expansion is charged (ADR 017) | `adversarial` group |
| H7 | `UnsafeDepth` exported through an exposed module | `Limits` has a hidden constructor; internals are `other-modules` | `custom limits are validated` |
| H8 | One error at a time; no `Exception`; golden tests pinned `show`; dead constructors | `ConfigErrors` (non-empty, `Semigroup`, `Exception`), renderer pinned by goldens, every `ErrorKind` constructor produced (ADR 025) | `errors (golden)` |
| H9 | Safe Haskell claims did not match the code | Pragmas removed (ADR 022) | — |

## Medium

| # | Finding | Resolution |
|---|---|---|
| M1 | `Path` was a list of text keys (index/key confusion, O(n²) snoc) | `Seq Segment` with `Key`/`Index`; `renderPath`/`parsePath` round-trip |
| M2 | Arrays as lists | `Vector` |
| M3 | Key order and number kinds | Keys are sorted (Rust's default ordering); numbers are exact `Scientific`, plus explicit non-finite values; integral decoders accept integral values (`1.0` included), never truncate, and never wrap |
| M4 | Lazy fields | All value, tag and error fields strict |
| M5 | Top level not required to be a dictionary | Enforced with a precise error |
| M6 | Null semantics implicit | Documented in `Figment.Merge` (Rust semantics: `null` is a value) |
| M7 | Copy-pasted `buildRaw` | One `Internal.Nest` |
| M8 | Lazy IO, stringly `SourceId`, no call site | Strict reads; structured `Source`; `HasCallStack` site per provider; interpolater |
| M9 | CLI doc/behaviour mismatch, no flags | Revised rule (ADR 020) |
| M10 | Dead type-level code | Removed with `Figment.Check` |
| M11 | Global CPP/DeriveAnyClass, heavy tool dep, dishonest bounds, stale freeze | GHC2021 with explicit strategies; no CPP; no `build-tool-depends`; bounds equal tested compilers; freeze removed (ADR 016) |
| M12 | CI skipped compile-fail and benchmarks; junk files | Both are CI steps; `failure` removed; study file renamed |

## Documentation drift

Every ADR that 0.2 changes now carries a status note at the top, and
ADRs 016–025 record the new decisions. The README describes only
behaviour that the tests exercise.

## Not done (and why)

- **hlint / formatter in CI.** No hlint binary exists for the platform
  this release was prepared on, so a lint gate could not be run locally
  before adding it. Recommended next step: add a `haskell-actions/hlint-run`
  job and fourmolu once the code has been formatted once.
- **GHC 9.12.4.** Declared in `tested-with` and run by CI; the release
  was verified locally on 9.10.3 only.
- **Positions for JSON.** aeson's token stream does not expose offsets.
  JSON errors carry path and source, but no line or column.
- **Insertion-ordered dictionaries.** Keys are sorted, as in Rust
  figment's default. A `preserve_order` equivalent can be added later
  without breaking the API.
