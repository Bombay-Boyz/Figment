# Contributing

Thanks for helping. Before opening a pull request:

1. `cabal build all --enable-tests --flags=dev` (the `dev` flag turns on `-Werror`).
2. `cabal test --flags=dev` and `bash scripts/check-compile-fail.sh`.
3. If you change behaviour, add a regression test named after the defect and
   a line to `CHANGELOG.md`.
4. Read `ENGINEERING_STANDARD.md`. A deliberate deviation needs a short ADR in
   `ADRs/`.
5. Anything that changes an exported signature, adds a constructor to a public
   sum type, or adds an instance follows the PVP (major bump).

Security problems: see `SECURITY.md`, not a public issue.
