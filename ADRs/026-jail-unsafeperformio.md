# ADR 026 — The one `unsafePerformIO`: the jail's process-wide lock

Status: Accepted | Rule relaxed: ENGINEERING_STANDARD §1.7 (no `unsafePerformIO`)

## Context
`Figment.Jail.withJail` changes the process working directory and
environment, which are global. Concurrent jails would corrupt each other, so
they must be serialised by a lock that exists once per process.

## Decision
`Figment.Jail` defines its lock with the standard top-level
`unsafePerformIO (newMVar ())` and a `NOINLINE` pragma. It is the only use in
the library (`.hlint.yaml` allows it in that module and nowhere else).

## Alternatives tried
- Passing a lock in as an argument: every test would have to share one value,
  which defeats the purpose of a drop-in `withJail`.
- `System.IO.Unsafe` free designs (file locks): heavier, platform dependent,
  and no more correct within one process.

## Consequences
The idiom is safe because the initialiser has no inputs and `NOINLINE`
guarantees one instance. The cleaner fix is to move `Jail` out of the main
library into a test-support sublibrary, where the exception would no longer
reach production users; that is tracked as a follow-up.
