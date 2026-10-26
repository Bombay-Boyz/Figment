# ADR 017 — Resource limits are enforced inside the parsers
Status: Accepted | Date: 2026-10-22 | Amends: 001

## Context
0.1 checked depth in `mkTagged`, after aeson, yaml or toml-parser had
already built the whole tree, so the guard ran after the cost had been
paid. Its stress test built a lazy tree the guard never forced, so it
proved nothing about real input.

## Decision
`Limits { maxDepth = 64, maxNodes = 1_000_000 }` (smart constructor
`mkLimits`; defaults per ADR 001) are threaded into every parser and
checked while the tree is built:
- JSON: aeson's lazy token stream, so tokens past the limit are never
  produced;
- YAML: the libyaml event stream; an alias is charged its *expanded*
  size and height (this defeats "billion laughs");
- TOML: the converter from toml-parser's tree (toml-parser's LR parser
  keeps its stack on the heap, which grows only with the input);
- env/CLI structured values: the value parser.

## Verification
`test/Figment/Test/Adversarial.hs`: 1M-deep JSON text, 100k-deep YAML,
10k-deep TOML and env values are rejected with `DepthExceeded 64`; an
exact boundary test (64 accepted, 65 rejected); a billion-laughs document
is rejected with `TooManyNodes`; arbitrary bytes and truncated documents
never raise an exception (results are forced with `evaluate . force`).
