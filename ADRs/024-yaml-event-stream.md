# ADR 024 — YAML from libyaml events, YAML 1.2 core schema
Status: Accepted | Date: 2026-10-22

## Decision
Parse the libyaml event stream directly instead of going through
`Data.Yaml`. This gives positions for every value, duplicate-key errors,
alias expansion charged to the node limit, merge keys (`<<`, where
explicit keys win), and an explicit rejection of multi-document files.
Plain scalars resolve under the YAML 1.2 core schema, so `no`, `on` and
`yes` remain strings. Explicit tags (`!!str`, `!!int`, ...) are honoured.
