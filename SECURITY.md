# Security policy

figment reads configuration from files, environment variables and command-line
arguments, so reports about hostile or malformed input are in scope: memory or
time exhaustion in a parser, an exception escaping `load`, forged or misleading
error output, and disclosure of values in error messages.

## Supported versions

Only the latest release on Hackage receives fixes.

## Reporting a vulnerability

Please do not open a public issue. Use GitHub's private reporting:
<https://github.com/Bombay-Boyz/Figment/security/advisories/new>
or email the maintainer address in `figment.cabal`.

Include the figment version, GHC version, a minimal input that reproduces the
problem, and what you observed (for resource exhaustion, peak memory from
`+RTS -s` is ideal). You will get an acknowledgement; fixes are released as a
new version with a changelog entry.
