#!/usr/bin/env bash
# Verifies that the CompileFail fixtures behave as §6.2 requires.
# Run from the repo root after `cabal build all --enable-tests` has
# succeeded, so the .ghc.environment.* file exists and `figment` is
# visible to a bare `ghc` invocation.
set -euo pipefail

check_fails() {
  local fixture="$1"
  local expected="$2"
  local out
  out=$(ghc -fno-code "$fixture" 2>&1 || true)
  if ! grep -q "$expected" <<<"$out"; then
    echo "FAIL: $fixture did not fail with '$expected'"
    echo "$out"
    exit 1
  fi
  echo "ok: $fixture fails as expected"
}

check_compiles() {
  local fixture="$1"
  if ! ghc -fno-code "$fixture" 2>&1; then
    echo "FAIL: $fixture did not compile"
    exit 1
  fi
  echo "ok: $fixture compiles"
}

check_fails   test/CompileFail/NoProvider.hs  "Figment: no provider"
check_compiles test/CompileFail/OkProvider.hs
