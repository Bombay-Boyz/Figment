# ADR 019 — Environment variables follow Rust figment (supersedes 008)
Status: Accepted | Date: 2026-10-22

## Decision
- The prefix is matched case-insensitively and stripped. Key
  transformations (`split`, `only`, `ignore`, `filterKeys`, `mapKeys`)
  apply in the order they were added. Keys are lower-cased by default.
  As in Rust, a `.` in the final key nests, and a key with an empty
  segment is ignored.
- **No splitting by default**: `APP_MAX_CONNECTIONS` is
  `max_connections`. `split "__"` makes `APP_DB__HOST` `db.host`. (0.1
  always split on `_`, which made underscored field names unreachable.)
- Values are parsed (TOML-like: booleans, exact numbers, quoted strings,
  arrays, inline dictionaries; anything else is the string itself) and
  keep their original text.
- Variables are processed in sorted order. Two variables that cannot
  both hold (a value and a dictionary at the same key, or two names that
  are equal after lower-casing) are **errors** naming both variables;
  0.1 silently dropped one (and, through a bug, dropped every value).
- `Env.fromList` injects variables, so tests need no process-global
  state.
