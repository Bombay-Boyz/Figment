# ADR 020 — Command-line rule, revised (supersedes 010)
Status: Accepted | Date: 2026-10-22

## Decision
`--key=value` (value parsed as for env vars), `--flag` (true),
`--no-flag` (false); dots nest, dashes become underscores, case is
kept; `--` ends options; everything else is positional and returned by
`parseArgs`. A repeated key: the last occurrence wins (the CLI
convention). Malformed options and structural conflicts are errors.
`--key value` is not supported: without a schema it is ambiguous with a
flag followed by a positional argument. Rich CLIs should parse with
optparse-applicative and pass the result through `defaults`, which is
what Rust figment recommends with clap.
