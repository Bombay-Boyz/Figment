#!/usr/bin/env bash
set -euo pipefail

check_fails() {
  local fixture="$1"
  local expected="$2"
  local out
  out=$(cabal exec -- ghc -fno-code "$fixture" 2>&1 || true)
  if ! grep -q "$expected" <<<"$out"; then
    echo "FAIL: $fixture did not fail with '$expected'"
    echo "$out"
    exit 1
  fi
  echo "ok: $fixture fails as expected"
}

check_compiles() {
  local fixture="$1"
  if ! cabal exec -- ghc -fno-code "$fixture" 2>&1; then
    echo "FAIL: $fixture did not compile"
    exit 1
  fi
  echo "ok: $fixture compiles"
}

check_fails   test/CompileFail/NoProvider.hs  "Figment: no provider"
check_compiles test/CompileFail/OkProvider.hs
