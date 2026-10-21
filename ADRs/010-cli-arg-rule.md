# ADR 010 — CLI argument decomposition rule
Status: Accepted | Date: 2026-10-21 | Rule: §2.8

## Decision
A CLI argument is consumed as @--key=value@. The key is lowercased and
split on '.' into path components. Arguments not starting with '--' or
with no '=' are ignored.

## Consequences
- Test/Figment/CliProvider.hs pins each case.
- Space-separated form @--key value@ is NOT supported at this layer;
  argument-pairing is the caller's job before passing to 'CliArgs'.
