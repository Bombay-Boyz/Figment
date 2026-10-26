# ADR 011 — Benchmark CI: baseline artifact, no auto-fail thresholds yet

> **Amended (0.2.0.0): CI builds the benchmarks and does a smoke run (`--time-limit 0.2`) on every build. Threshold-based failing is still deferred; the "future ADR 012" mentioned below was never written (that number went to provenance).**

Status: Accepted (provisional) | Date: 2026-10-21 | Rule: §5.6

## Context
§5.6 requires benchmarks checked into CI with regression thresholds.
Thresholds must be calibrated against real variance; without a baseline
sample, a threshold is a guess.

## Decision
- Run the benchmark suite on every PR (quick mode, in the `build` job).
- Run the full benchmark on `main` only, upload the JSON result as an
  artifact, and promote it to `bench-baseline` on every successful main
  build.
- Do NOT auto-fail a PR on benchmark regression until at least 10
  baseline samples exist and variance is measured.

## Rationale
An auto-fail threshold set without variance data would be either
toothless (too high) or flaky (too low). The baseline artifact provides
the data needed for a future ADR that sets real thresholds.

## Consequences
- Benchmark results are visible in CI artifacts on every main build.
- A future ADR 012 will introduce threshold-based failing.
- Until then, PR authors are expected to inspect the artifact manually.
