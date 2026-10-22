Layered configuration merging for Haskell, with per-node provenance and
a compile-time check that a provider chain can actually supply values.

`figment` reads configuration from JSON, TOML, YAML, environment
variables, CLI arguments, or caller-supplied defaults, merges them under
a chosen strategy, and decodes the result into a record. Every node in
the merged tree carries its source, so you can always answer *"which
layer set this value?"*

## At a glance

```haskell
{-# LANGUAGE DataKinds, OverloadedStrings, TypeApplications #-}

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Figment

data AppConfig = AppConfig
  { host :: String
  , port :: Int
  } deriving (Eq, Show, Generic)
instance FromJSON AppConfig

loadApp :: IO (Either ConfigError AppConfig)
loadApp = do
  layers <- sequence
    [ fmap Join  <$> tagProviderDefault (Defaults (AppConfig "localhost" 8080))
    , fmap Merge <$> tagProviderDefault (TomlFile "config.toml")
    , fmap Merge <$> tagProviderDefault (EnvPrefixed "APP_")
    ]
  pure $ do
    ls <- sequence layers
    composeChecked @AppConfig @'[TomlFile, EnvPrefixed] ls
What it guarantees
Depth-bounded tagging. Every provider funnels untrusted input
through mkTagged, which fails with DepthExceeded at a caller-supplied
recursion budget instead of exhausting the stack. The default budget is
64; it is overridable via tagProvider someDepth.

Closed error vocabulary. Every failure is a constructor of
ConfigError, one per pipeline stage, each carrying the path where the
failure occurred. No error calls, no untyped exceptions escaping.

Merge algebra with documented laws. Four strategies
(merge/join/admerge/adjoin) are instances of one algebra. The
associativity law holds under schema-uniformity; it is tested as a
property, not assumed. There is deliberately no Semigroup instance,
because mixed-strategy chains are not associative.

Safe Haskell boundary. The pure core (Value, Merge, Layer,
Error, Prim) is Safe — GHC has verified it contains no
unsafePerformIO and cannot violate referential transparency. The IO
adapters are Trustworthy (ADR 014).

Provenance-preserving extraction. runExtractProv returns the
decoded record and a Path → Metadata map, so callers can ask which
layer supplied any given leaf (ADR 012).

Providers
Type	Supply source	Notes
JsonFile	JSON file	via aeson
TomlFile	TOML file	via toml-parser; invalid UTF-8 is rejected as SourceError
YamlFile	YAML file	via yaml
EnvPrefixed	Process environment	reads vars with a prefix, lower-cases, splits on _ (ADR 008)
CliArgs	CLI arguments	--key=value, .-separated nested keys (ADR 010)
Defaults a	Caller-supplied record	via aeson's ToJSON
FieldProvider p	Wrap any custom provider	extension point for third-party providers
Dhall is deliberately not shipped (see ADR 011).

Required and optional fields
figment does not do field-level coverage checking at the type level —
that would require guessing what a file provider contains, which is not
possible (ADR 013). Instead, the discipline matches Rust's figment:

Required fields are any field in your record that has no default.
If no provider supplies them, runExtract returns
DecodeError/MissingKey.

Optional fields are Maybe fields, or fields with FromJSON
defaults. They decode to Nothing / their default when absent.

composeChecked only checks that at least one provider in the chain
can supply non-file values. That is the compile-time guarantee it makes.

Two extraction functions
haskell
runExtract     :: FromJSON a => Tagged -> Either ConfigError a
runExtractProv :: FromJSON a => Tagged -> Either ConfigError (a, ProvMap)
Use runExtract when you want the record. Use runExtractProv when you
want the record plus the ability to ask, for any leaf path, which layer
supplied it. Both go through the same aeson FromJSON decode; the
provenance map is built in a second walk of the merged tree.

Compile-time reachability
composeChecked @R @ps requires ps to contain at least one provider
that can supply non-file values — EnvPrefixed, CliArgs,
Defaults R, or a FieldProvider-wrapped custom provider. If not, the
call fails with a single named TypeError, not an opaque kind mismatch:

text
Figment: no provider in this chain can supply a field that has no
file-based value - add EnvPrefixed, CliArgs, Defaults, or wrap a custom
provider in FieldProvider.
For deliberately file-only configurations, use composeUnchecked.

Status
Builds and passes 57 tests on GHC 9.12.1. (GHC 9.14.1 is
deferred until hashable supports base-4.22 — see ADR 015.)

Version: 0.1.0.0 (pre-release).

License: BSD-3-Clause. See LICENSE.

Design
The library is governed by ENGINEERING_STANDARD.md (in this repo) and
was designed against the feasibility study that accompanies this
release. Every non-obvious decision is recorded in ADRs/:

ADR	Decision
001	defaultDepth = 64
002	CPP guard for foldl' import
003	Manual NFData instances
004	cabal.project.freeze deferred to CI
005	QuickCheck < 2.20 bound
006	runExtract two-pass decode (superseded by 012)
007	-Wno-redundant-constraints for composeChecked
008	Env-var key decomposition rule
009	Orphan Provider instances in adapter modules
010	CLI argument decomposition rule
011	Benchmark CI: baseline artifact, no auto-fail thresholds
012	runExtractProv — provenance-preserving extraction
013	Field-level coverage is a runtime concern, not type-level
014	Safe Haskell boundary: Safe core, Trustworthy adapters
015	GHC 9.14.1 support deferred pending hashable
Building
bash
cabal build all --enable-tests --enable-benchmarks
cabal test all
cabal bench all
Requires GHC 9.10.3 or 9.12.4. These are the only versions verified
by the maintainer and tested in CI.
EOF
