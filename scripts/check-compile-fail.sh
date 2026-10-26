#!/usr/bin/env bash
# Compile-time guarantees, checked mechanically (run from the package
# root after `cabal build`). CABAL may be overridden (CI passes its own).
set -euo pipefail
CABAL=${CABAL:-cabal}

typecheck() {
  $CABAL exec -v0 -- ghc -fno-code -package figment -package text "$1" 2>&1
}

expect_failure() {
  local fixture="$1" expected="$2" out
  if out=$(typecheck "$fixture"); then
    echo "FAIL: $fixture compiled, but must not"; exit 1
  fi
  if ! grep -q -- "$expected" <<<"$out"; then
    echo "FAIL: $fixture failed without the expected message '$expected':"; echo "$out"; exit 1
  fi
  echo "ok: $fixture is rejected ($expected)"
}

expect_success() {
  local fixture="$1" out
  if ! out=$(typecheck "$fixture"); then
    echo "FAIL: $fixture must compile:"; echo "$out"; exit 1
  fi
  echo "ok: $fixture compiles"
}

expect_success test/compile-fail/Ok.hs
expect_failure test/compile-fail/EmptyType.hs "cannot derive FromConfig for an empty data type"
expect_failure test/compile-fail/PartialSum.hs "a Partial configuration must be a record with a single constructor"
expect_failure test/compile-fail/NoValueSemigroup.hs "No instance for .Semigroup Value"
