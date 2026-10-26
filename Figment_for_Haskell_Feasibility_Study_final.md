# Figment for Haskell: A Formal Feasibility Study

**Governing document:** `haskell-engineering-standard.md` is binding on
every design decision below. Each architectural claim is backed by a
proof, a compile-time check, or a named test, as Part 5 of the Standard
requires (Standard §2.8).

**Verification status.** The Haskell in §1, §5–§8.4 and the `fromAeson`
boundary of §8.5 has been compiled with GHC 9.4.7, and the algebraic
claims of §3 have been run as QuickCheck properties. §10.1 states
precisely what was machine-checked and what was not. The Dhall provider,
`runExtract`, and the TOML/YAML/Env/CLI providers are not yet verified or
implemented.

---

## 1. The Value Domain

### 1.1 Values and Tags

```
v ∈ 𝕍 ::= Null
         | Bool  b
         | Num   n
         | Str   s
         | Arr   [τ]                 -- elements are tagged, recursively
         | Obj   (Key ⇀ τ)           -- values are tagged, recursively

τ ∈ 𝕋 = 𝕍 × 𝕄
```

`𝕍` and `𝕋` are mutually recursive: every node in a configuration tree,
not only the root, carries its own metadata. This is required for the
provenance guarantee of §4 to be representable at all — a guarantee about
"the tag of an arbitrary leaf" is vacuous unless leaves *have* tags
independently of their ancestors.

### 1.2 Metadata

```
𝕄     = Maybe (SourceId, Profile, Path)
Path  = [Key]                          -- root-to-node key sequence
```

`Nothing` denotes a **synthetic** tag (§2's constant-synthesis rule);
`Just (s, p, path)` denotes an **authoritative** tag placed by exactly one
provider. `Key`, `SourceId`, and `Path` are three distinct domain concepts
that happen to share an underlying `Text`, and are therefore three
`newtype`s rather than one repeated `Text` (Standard §3.2):

```haskell
newtype Key      = Key      Text deriving (Eq, Ord, Show)
newtype SourceId = SourceId Text deriving (Eq, Ord, Show)
newtype Path     = Path     [Key] deriving (Eq, Show)

-- | Open-ended, user-named variant ("dev", "staging", ...); not a closed
-- enumeration this library owns (Standard §1.4). 'DefaultProfile' is
-- explicit so "no profile selected" is never an empty 'Text'.
data Profile = DefaultProfile | NamedProfile ProfileName
  deriving (Eq, Show)
newtype ProfileName = ProfileName Text deriving (Eq, Ord, Show)
```

`lookupDoc`-style accidental swaps (Standard §3.2's own example) are
compile errors: a `SourceId` cannot be passed where a `Key` is expected,
and vice versa, at zero runtime cost.

### 1.3 Tagging, with an explicit recursion bound

Every provider (§8.2) feeds *untrusted, externally supplied* data through
tagging — exactly the case Standard §2.4 names: "any recursive data type
that hostile or careless input could nest arbitrarily deep gets an
explicit, threaded depth/size limit at every point of recursive descent,
applied uniformly across every stage capable of producing that nesting."
A maliciously or accidentally deep JSON/TOML/YAML/Dhall document must
fail with a typed error, not exhaust the stack.

```haskell
-- | A strictly positive recursion budget; 'mkDepth' is the only way to
-- obtain one (Standard §1.3, §3.3).
newtype Depth = UnsafeDepth Int deriving (Eq, Ord, Show)

mkDepth :: Int -> Either ConfigError Depth
mkDepth n
  | n > 0     = Right (UnsafeDepth n)
  | otherwise = Left (ConfigurationError "Depth" "must be a positive Int")

-- | Default budget for built-in providers. 64 comfortably exceeds any
-- hand-authored config schema while bounding worst-case stack use to a
-- small constant (Standard §4.10).
defaultDepth :: Depth
defaultDepth = UnsafeDepth 64

-- | Provider-agnostic untagged tree into which every parser's AST is
-- flattened before tagging (Standard §2.1).
data RawJSON
  = RawNull
  | RawBool Bool
  | RawNum  Scientific
  | RawStr  Text
  | RawObj  [(Text, RawJSON)]
  | RawArr  [RawJSON]

-- | Tags every node with its provenance. Fails with 'DepthExceeded',
-- carrying the caller's budget, if nesting exceeds that budget. Structural
-- recursion on a strictly smaller 'RawJSON' with a strictly decreasing
-- budget, so it terminates. This is the only place 'Path' is computed; the
-- budget is threaded by hand because no existing combinator both
-- decrements a bound and builds a value.
mkTagged :: Depth -> (SourceId, Profile) -> RawJSON -> Either ConfigError Tagged
mkTagged total@(UnsafeDepth budget) (s, p) = go budget (Path [])
  where
    go remaining path raw
      | remaining <= 0 = Left (DepthExceeded path total)
      | otherwise = case raw of
          -- The fmap chains are parenthesized: `taggedAs` defaults to
          -- infixl 9 and would otherwise bind tighter than '<$>'.
          RawObj fields ->
            (VObj . Map.fromList <$>
              traverse (\(k, v) ->
                          (,) (Key k) <$> go (remaining - 1) (extend path k) v)
                       fields)
              `taggedAs` path
          RawArr xs ->
            (VArr <$>
              traverse (\(i, v) -> go (remaining - 1) (extendIx path i) v)
                       (zip [0 :: Int ..] xs))
              `taggedAs` path
          RawNull   -> pure VNull      `taggedAs` path
          RawBool b -> pure (VBool b)  `taggedAs` path
          RawNum  n -> pure (VNum n)   `taggedAs` path
          RawStr  t -> pure (VStr t)   `taggedAs` path
      where
        taggedAs mv p' = (\v -> Tagged v (Leaf s p p')) <$> mv
```

(`extend`/`extendIx` append a `Key`/index to a `Path`; in the real module
both are `O(1)` amortized `Seq`-backed appends (Standard §4.3), elided
here because they add no algebraic content.) Every provider goes through
`mkTagged`, so the depth bound is uniform across JSON, TOML, YAML, and
Dhall ingestion, as §2.4 requires.

**The aeson boundary.** `Data.Aeson`'s object type (`Object`, a `KeyMap
Value` as of aeson ≥ 2.0) has its own key type, `Data.Aeson.Key.Key` — a
different type from this library's `Key` newtype (§1.2) despite the shared
name. `Provider/Json.hs` defines the single conversion function
`fromAeson :: Aeson.Value -> RawJSON` (shown in §8.5, where the Dhall
provider also uses it), which unwraps aeson's `Key` to `Text` in exactly
one place. Nothing downstream of `RawJSON` sees aeson's `Key`, so the name
collision is contained to one function (Standard §0.8).

**Merge needs no bound of its own.** `⊕`/`mergeAlg` (§2) combines two
already-`mkTagged` trees pairwise by unioning siblings at matching depths,
so the depth of a merged tree is the `max` of the two operands' depths,
never their sum. A chain of `n` layers, each bounded by `defaultDepth`,
therefore yields a result bounded by `defaultDepth` regardless of `n`.
This is stated as a property in §10.

---

## 2. The Merge Operator, `⊕`


```
⊕ : Bias → ArrayPolicy → 𝕋 → 𝕋 → 𝕋

(v₁, m₁) ⊕[bias,ap] (v₂, m₂) =
  case (v₁, v₂) of
    (Obj o₁, Obj o₂) →
      ( Obj (unionWith (λ τa τb → τa ⊕[bias,ap] τb) o₁ o₂)
      , Nothing )                    -- synthetic: no single origin
    (Arr a₁, Arr a₂) | ap = Concat →
      ( Arr (a₁ ++ a₂), Nothing )    -- synthetic, same reasoning
    (Arr a₁, Arr a₂) | ap = Replace →
      pick bias (Arr a₁, m₁) (Arr a₂, m₂)
    (_, _) →
      pick bias (v₁, m₁) (v₂, m₂)

pick RightWins x y = y
pick LeftWins  x y = x
```

Merged `Obj` and concatenated `Arr` nodes always receive the constant
synthetic tag `Nothing`, never one operand's real tag. This is what makes
Lemma 3 an *exact* tree equality rather than one up to container-tag
noise.

The four named strategies instantiate the skeleton above:


| Strategy | Bias | Array policy |
|---|---|---|
| `merge`   | RightWins | Replace |
| `join`    | LeftWins  | Replace |
| `admerge` | RightWins | Concat  |
| `adjoin`  | LeftWins  | Concat  |

---

## 3. Algebraic Properties

The proofs are representation-independent: they reason about `shape` (the
constructor of a value — `Null`, `Bool`, `Num`, `Str`, `Arr`, or `Obj`),
`pick`, and `unionWith`.


### Lemma 1 (Identity, restricted to `Obj`-shaped operands)

Let `ε = (Obj ∅, Nothing)`. For any `τ` whose value component is `Obj _`:

```
fst (τ ⊕[RightWins,ap] ε) = fst τ         and         fst (ε ⊕[LeftWins,ap] τ) = fst τ
```

**Proof.** Write `τ = (Obj o, m)`. Since `unionWith f o ∅ = o`, the value
component matches on both sides; the tag component is `Nothing` on both
sides by the constant-synthesis rule of §2, so the law is stated at the
value level only — a container merged against the identity loses the
"this object is untouched" fact at the top level, in exchange for the
container tag being provably meaningless (`Nothing`, always) rather than
an arbitrary, possibly misleading, one-operand provenance. ∎

**The restriction to `Obj`-shaped `τ` is load-bearing, not cosmetic.**
`(Num 1, m) ⊕[RightWins,ap] (Obj ∅, Nothing)` falls to `pick RightWins`,
returning the empty object, **not** `τ`. There is no two-sided identity
for the full `𝕍` domain, and none is needed — see schema-uniformity
under Lemma 3.

### Lemma 2 (Right-bias is not commutative)

`Num 1 ⊕[RightWins,ap] Num 2 = Num 2 ≠ Num 1 = Num 2 ⊕[RightWins,ap] Num 1`,
by direct evaluation of `pick RightWins`. ∎

### Lemma 3 (Associativity, under schema-uniformity)

**Definition (schema-uniform triple).** Trees `τ₁, τ₂, τ₃` are
**schema-uniform** if, for every key-path `p` reachable in any of the
three trees, whenever two of the three trees both define a value at `p`,
those two values have the same `shape`. Trees decoded into the same
target record type `R` are automatically schema-uniform.

**Statement.** For a fixed `(bias, ap)` and a schema-uniform triple:
`(τ₁ ⊕ τ₂) ⊕ τ₃ = τ₁ ⊕ (τ₂ ⊕ τ₃)` as full trees, values and tags, exactly.

**Proof.** By strong induction on combined tree size.
- *All three `Obj`-shaped*: both groupings reduce key-by-key via
  `Data.Map.unionWith`; a key in all three reduces to `f (f x y) z` vs.
  `f x (f y z)`, equal by the inductive hypothesis; a key in only one or
  two reduces, by inspection of `unionWith`, to the same operand(s)
  regardless of grouping. Container tag is `Nothing` under both groupings
  by construction.
- *All three `Arr`-shaped, `ap = Concat`*: list concatenation is
  associative; tag is `Nothing` under both groupings.
- *All three agree in a fixed scalar shape (or `Arr` under `Replace`)*:
  `pick bias` always returns the same positional operand irrespective of
  grouping, tag included. ∎

**Necessity of schema-uniformity.** `τ₁ = Arr [Num 1]`,
`τ₂ = Obj {"a" ↦ Num 1}`, `τ₃ = Arr [Num 2]`, `RightWins`/`Concat`: left
association yields `Arr [Num 2]`; right association yields
`Arr [Num 1, Num 2]`. Two different, both well-formed results, with no
decode-time error in either branch — shape mismatches produce silent,
undetected divergence, which is exactly why §7's `runLayers` is a
non-reassociable left fold, never a `Semigroup`.

### Corollary (Mixed-strategy chains are not associative)

A sequence of layers combined under different strategies must be
evaluated as an explicit, ordered left fold — never exposed as an
associative operation, never given a `Semigroup`/`Monoid` instance a
caller could legally reassociate (Standard §0.3: instances that don't
obey the laws of their class are the Haskell analogue of an LSP
violation — the fix here is to never offer the instance in the first
place, not to offer an unlawful one).

---

## 4. Provenance Propagation


**Theorem.** For any key `k` present in a merged result, if the value at
`k` is itself a leaf, its tag is `Leaf s p path`, where `path` was
assigned once, at tagging time (§1.3), by whichever provider's value was
last selected under §2's resolution rule, and `path` is exactly the key
sequence from the root to `k`.

**Proof.** `pick` returns one full `Tagged` value together — it never
splices a value from one operand with metadata from the other. The
`Obj/Obj` case of `⊕` only overwrites the tag at the *container* level;
metadata already attached to nested values is exactly the `m` threaded
through recursive calls, unchanged except for whichever `pick` runs at
that sub-path. Induction on tree depth: depth 0 (a leaf) holds trivially,
since a leaf's tag *is* its path by §1.3; at depth `n+1`, the value at
`k` is the result of `⊕` on the two sub-values already stored at `k`,
both correctly path-stamped by `mkTagged`'s recursion — whichever `pick`
selects still carries the correct path. ∎

---

## 5. Correspondence to Haskell

```haskell
data Value
  = VNull
  | VBool   Bool
  | VNum    Scientific
  | VStr    Text
  | VArr    [Tagged]
  | VObj    (Map Key Tagged)

data Metadata
  = Merged
  | Leaf { source :: SourceId, profile :: Profile, path :: Path }

data Tagged = Tagged { tVal :: Value, tMeta :: Metadata }

data Bias        = LeftWins | RightWins  deriving (Eq, Show)
data ArrayPolicy = Replace  | Concat     deriving (Eq, Show)

-- | Three-way pattern match on (v1, v2, ap). 'ArrayPolicy' is a closed
-- sum, so GHC checks exhaustiveness and no 'Eq ArrayPolicy' constraint is
-- needed (Standard §0.10).
mergeAlg :: Bias -> ArrayPolicy -> Tagged -> Tagged -> Tagged
mergeAlg bias ap t1@(Tagged v1 _) t2@(Tagged v2 _) = case (v1, v2, ap) of
  (VObj o1, VObj o2, _) ->
    Tagged (VObj (Map.unionWith (mergeAlg bias ap) o1 o2)) Merged
  (VArr a1, VArr a2, Concat) ->
    Tagged (VArr (a1 ++ a2)) Merged
  (VArr _, VArr _, Replace) ->
    pick bias t1 t2
  (_, _, _) ->
    pick bias t1 t2

pick :: Bias -> a -> a -> a
pick LeftWins  x _ = x
pick RightWins _ y = y
```

`Value`/`Tagged` are mutually recursive, mirroring `𝕍`/`𝕋` exactly, and
`Metadata`'s two constructors mirror `𝕄 = Maybe (...)` exactly
(`Merged ↔ Nothing`, `Leaf {..} ↔ Just (s,p,path)`).

---

## 6. Structural Reachability

The check is witnessed by *closed* type families (§6.2), which the
Standard's §0.2 names as the correct tool "when the set of cases is owned
and finite". The built-in generic providers form such a set, with
`FieldProvider p` as the explicit extension point for third-party
providers (Standard §2.7: the closed-versus-open choice is stated per
axis).

### 6.1 Claim and scope

`composeChecked @r @ps` compiles only if `ps` contains at least one
provider that can supply values for fields with no file-based value:
`EnvPrefixed`, `CliArgs`, `Defaults r` (for the same `r` being composed),
or a `FieldProvider`-wrapped provider. Otherwise it fails with a single,
named `TypeError`.

Scope:

1. The check is over the provider list, not over individual fields. It
   establishes that *some* provider could supply non-file values, not that
   every required field is covered. Field-level coverage builds on the
   families of §8.4 and is Gate 2(d) work (§12).
2. It is a "could supply" check. Whether a provider actually supplies a
   required value at run time is reported by `MissingKey` (§8.1), a
   run-time error distinct from this compile-time one.
3. Deliberately file-only configuration uses `composeUnchecked` (§8.2).

### 6.2 Type families

```haskell
{-# LANGUAGE DataKinds, TypeFamilies, PolyKinds, UndecidableInstances,
             AllowAmbiguousTypes, TypeApplications, ScopedTypeVariables,
             ConstraintKinds, TypeOperators, KindSignatures #-}
import Data.Kind (Type, Constraint)
import GHC.TypeLits (TypeError, ErrorMessage(..))

type family IsGenericProvider (p :: Type) (r :: Type) :: Bool where
  IsGenericProvider EnvPrefixed       r = 'True
  IsGenericProvider CliArgs           r = 'True
  IsGenericProvider (Defaults r)      r = 'True
  IsGenericProvider (FieldProvider p) r = 'True
  IsGenericProvider p                 r = 'False

type family Or (a :: Bool) (b :: Bool) :: Bool where
  Or 'True  b = 'True
  Or 'False b = b

type family AnyGeneric (ps :: [Type]) (r :: Type) :: Bool where
  AnyGeneric '[]       r = 'False
  AnyGeneric (p ': ps) r = Or (IsGenericProvider p r) (AnyGeneric ps r)

type family RequireGenericProviderC (b :: Bool) :: Constraint where
  RequireGenericProviderC 'True  = ()
  RequireGenericProviderC 'False =
    TypeError
      ('Text "Figment: no provider in this chain can supply a field that "
       ':<>: 'Text "has no file-based value — add EnvPrefixed, CliArgs, "
       ':<>: 'Text "Defaults, or wrap a custom provider in 'FieldProvider'.")

type RequireGenericProvider (r :: Type) (ps :: [Type]) =
  RequireGenericProviderC (AnyGeneric ps r)
```

### 6.3 Termination and soundness

Every equation of `IsGenericProvider`, `Or`, `AnyGeneric`, and
`RequireGenericProviderC` is closed and total. `AnyGeneric` recurses
structurally on a closed, finite `ps`, terminating after `length ps`
steps. The `Defaults r` equation reuses the *same* `r` bound by the
family's second argument, so it fires only for `Defaults` at the record
actually being composed, not for an unrelated `Defaults r'` (the same
repeated-variable technique as `Elem` in §8.4).

---

## 7. Cross-Strategy Composition

```haskell
data Layer
  = Join    Tagged   -- LeftWins,  Replace
  | Merge   Tagged   -- RightWins, Replace
  | Adjoin  Tagged   -- LeftWins,  Concat
  | Admerge Tagged   -- RightWins, Concat

emptyObj :: Tagged
emptyObj = Tagged (VObj Map.empty) Merged

-- | The one place strategies are interpreted: a strict left fold,
-- matching the Corollary of §3. 'foldl'' rather than 'foldl' so an
-- unbounded chain of layers builds no thunk chain (Standard §4.10, §4.11).
runLayers :: [Layer] -> Tagged
runLayers = foldl' step emptyObj
  where
    step acc (Join    t) = mergeAlg LeftWins  Replace acc t
    step acc (Merge   t) = mergeAlg RightWins Replace acc t
    step acc (Adjoin  t) = mergeAlg LeftWins  Concat  acc t
    step acc (Admerge t) = mergeAlg RightWins Concat  acc t
```

There is no `Semigroup`/`(<>)` anywhere in this path: mixed chains cannot
be silently reassociated, by construction (Standard §2.8 — a mechanical,
not a documented, guarantee). Even for a single fixed strategy,
associativity holds only under schema-uniformity (Lemma 3), not for all
trees, so no `Semigroup` instance is offered; same-strategy associativity
is instead tested as a property (§10, Standard §5.5).

---

## 8. Public API and Providers

### 8.1 `Figment.Error` — the closed error vocabulary

`ConfigError` is a closed sum with one constructor per pipeline stage
(Standard §2.3), each naming the specific location that failed (§2.15),
defined in its own module (§1.5, §2.15):

```haskell
-- | Every way a configuration pipeline can fail, one constructor per
-- stage (Standard §2.3). A constructor is added only in the same change
-- that makes it reachable (Standard §1.5).
module Figment.Error (ConfigError(..)) where

data ConfigError
  = SourceError    FilePath Text
    -- ^ A provider's underlying I/O or parse step failed (e.g. §8.5's
    -- Dhall provider). 'Text' carries the upstream library's rendered
    -- message, 'FilePath' the offending source.
  | DepthExceeded  Path Depth
    -- ^ 'mkTagged' (§1.3) hit its recursion budget before reaching a
    -- leaf. 'Path' is the deepest key prefix reached; 'Depth' is the
    -- budget the caller supplied.
  | DecodeError    Path Text
    -- ^ 'runExtract' (§8.2) could not decode the merged tree into the
    -- target record at this path, e.g. a shape mismatch aeson caught.
  | MissingKey     Path
    -- ^ A required field had no provider actually supply a value at run
    -- time. Distinct from "no provider *could* supply it", which is a
    -- compile error (§6.1, scope note 2).
  | ValidationError Path Text
    -- ^ A smart constructor at the extraction boundary (e.g. 'mkPort',
    -- §8.3) rejected a well-typed but semantically invalid value; 'Text'
    -- names the rule violated.
  | ConfigurationError Text Text
    -- ^ A library-internal precondition was violated by the caller (e.g.
    -- 'mkDepth' given a non-positive budget). A programmer-error-shaped
    -- failure, still typed rather than an 'error' call (Standard §1.1).
  deriving (Eq, Show)
```

### 8.2 The `Provider` class and composition functions

`Depth` is threaded explicitly through `tagProvider`, so every provider's
recursion budget is caller-controllable; `tagProviderDefault` is the
zero-argument wrapper for the common case (Standard §0.4).

```haskell
class Provider p where
  tagProvider :: Depth -> p -> IO (Either ConfigError Tagged)

-- | The common case: tag with the library default budget.
tagProviderDefault :: Provider p => p -> IO (Either ConfigError Tagged)
tagProviderDefault = tagProvider defaultDepth

runExtract :: FromJSON a => Tagged -> Either ConfigError a

-- | Takes already-tagged 'Layer's, so it has no 'Depth' parameter: the
-- budget was fixed at the 'tagProvider' call that produced each 'Layer'.
composeChecked
  :: forall r ps
   . (Generic r, FromJSON r, RequireGenericProvider r ps)
  => [Layer] -> Either ConfigError r
composeChecked = runExtract . runLayers

-- | Escape hatch for deliberately file-only configuration (§6.1, scope
-- note 3). Identical run-time behaviour; no compile-time reachability
-- guarantee.
composeUnchecked :: FromJSON r => [Layer] -> Either ConfigError r
composeUnchecked = runExtract . runLayers
```

The depth override therefore happens one step earlier, at each
`tagProvider` call in §8.3's `configSource`.

**Calling `composeChecked`.** `ps` occurs only in the constraint, so it
cannot be inferred and must be supplied by type application (requires
`AllowAmbiguousTypes`, `TypeApplications`):

```haskell
cfg :: Either ConfigError AppConfig
cfg = composeChecked @AppConfig @'[TomlFile, EnvPrefixed] layers
```

Verified with GHC 9.4.7 (§10.1): this call site compiles; the same call
with `@'[TomlFile]` alone fails with the §6.2 `TypeError` text; adding
`Defaults AppConfig` or a `FieldProvider`-wrapped provider to the list
makes it compile again.

Built-in provider types are each `newtype`-wrapped over their
configuration (Standard §3.2: no bare `Text`/`FilePath` doing double duty
as "an env-var prefix" or "a file path"). The first four are declared in
`Provider.hs`, because the reachability check of §6 is defined over them;
the rest are declared in their adapter modules:

```haskell
-- Figment.Provider
newtype EnvPrefixed       = EnvPrefixed Text
newtype CliArgs           = CliArgs [Text]
newtype Defaults r        = Defaults r
newtype FieldProvider p   = FieldProvider p   -- extension point for third-party providers

instance Provider p => Provider (FieldProvider p) where
  tagProvider d (FieldProvider p) = tagProvider d p

-- One per adapter module
newtype TomlFile  = TomlFile  FilePath
newtype JsonFile  = JsonFile  FilePath
newtype YamlFile  = YamlFile  FilePath
newtype DhallFile = DhallFile FilePath
```

### 8.3 Example configuration source

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Figment
import Figment.Provider (EnvPrefixed(..), Defaults(..))
import Figment.Provider.Toml (TomlFile(..))

-- The caller's own record types. 'AppConfigRaw' is what 'runExtract'
-- decodes; 'AppConfig' is the validated form.
data AppConfigRaw = AppConfigRaw
  { rHost :: Text, rPort :: Int, rLogLevel :: Text, rDatabase :: Text }
  deriving (Generic, FromJSON)
data AppConfig = AppConfig Text Port Text Text

-- | The only way to obtain a 'Port', so downstream code can rely on the
-- range invariant (Standard §1.3).
newtype Port = UnsafePort Int deriving (Eq, Show)

mkPort :: Int -> Either ConfigError Port
mkPort n
  | n >= 1 && n <= 65535 = Right (UnsafePort n)
  | otherwise = Left (ValidationError (Path []) "port must be in 1..65535")

-- | Pass 'defaultDepth' for the common case, or a caller-chosen 'Depth'
-- when a schema is legitimately deeper than the library default.
configSource :: Depth -> [IO (Either ConfigError Layer)]
configSource d =
  [ fmap Join  <$> tagProvider d (Defaults defaultConfigRaw)
  , fmap Merge <$> tagProvider d (TomlFile "config.toml")
  , fmap Merge <$> tagProvider d (EnvPrefixed "APP_")
  ]

loadConfig :: IO (Either ConfigError AppConfig)
loadConfig = loadConfigWith defaultDepth

loadConfigWith :: Depth -> IO (Either ConfigError AppConfig)
loadConfigWith d = do
  layers <- sequence (configSource d)
  pure $ do
    ls  <- sequence layers
    raw <- runExtract (runLayers ls) :: Either ConfigError AppConfigRaw
    AppConfig (rHost raw) <$> mkPort (rPort raw)
              <*> pure (rLogLevel raw) <*> pure (rDatabase raw)
```

`mkPort`'s failure and every provider's failure share the single
`ConfigError` type (§8.1).

### 8.4 Compile-time field coverage: `Figment.Check`

A closed, total pair of type families with named `TypeError`s for every
reachable failure shape (unnamed fields, sum-typed "records"). `Elem` uses
the same repeated-variable pattern as the `Defaults r` equation of §6.2
(Standard §0.6). These families compile (§10.1); the constraint that
combines them with `ps` into a per-field coverage check is Gate 2(d) work.


```haskell
type family FieldsOf (rep :: Type -> Type) :: [Symbol] where
  FieldsOf (D1 _ f)  = FieldsOf f
  FieldsOf (C1 _ f)  = FieldsOf f
  FieldsOf (f :*: g) = FieldsOf f ++ FieldsOf g
  FieldsOf U1        = '[]
  FieldsOf (S1 ('MetaSel ('Just name) _ _ _) _) = '[name]
  FieldsOf (S1 ('MetaSel 'Nothing _ _ _) _) =
    TypeError ('Text "Figment: config records must use named fields.")
  FieldsOf (f :+: g) =
    TypeError ('Text "Figment: config records must have exactly one constructor.")

type family OptionalFields (rep :: Type -> Type) :: [Symbol] where
  OptionalFields (D1 _ f)  = OptionalFields f
  OptionalFields (C1 _ f)  = OptionalFields f
  OptionalFields (f :*: g) = OptionalFields f ++ OptionalFields g
  OptionalFields U1        = '[]
  OptionalFields (S1 ('MetaSel ('Just name) _ _ _) (Rec0 (Maybe a))) = '[name]
  OptionalFields (S1 _ _)  = '[]

type family (++) (xs :: [k]) (ys :: [k]) :: [k] where
  '[]       ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

type family Elem (k :: Symbol) (ks :: [Symbol]) :: Bool where
  Elem k '[]       = 'False
  Elem k (k ': ks) = 'True
  Elem k (j ': ks) = Elem k ks
```

### 8.5 Dhall as an opt-in provider

Dhall decodes through `dhall-json`'s `dhallToJSON` into an aeson `Value`,
then through `fromAeson`, which `Provider/Json.hs` shares. The
aeson↔`RawJSON` boundary is thus defined once rather than once per
file-format provider (Standard §0.6):

```haskell
-- Figment.Provider.Json (shared; JsonFile's instance also calls this on
-- the result of Data.Aeson.decodeFileStrict)
import qualified Data.Aeson        as Aeson
import qualified Data.Aeson.Key    as Aeson.Key
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.Vector       as Vector

-- | Unwraps aeson's own 'Key' type to 'Text' here and nowhere else (§1.3).
fromAeson :: Aeson.Value -> RawJSON
fromAeson Aeson.Null       = RawNull
fromAeson (Aeson.Bool   b) = RawBool b
fromAeson (Aeson.Number n) = RawNum  n
fromAeson (Aeson.String t) = RawStr  t
fromAeson (Aeson.Array  a) = RawArr (map fromAeson (Vector.toList a))
fromAeson (Aeson.Object o) =
  RawObj [ (Aeson.Key.toText k, fromAeson v)
         | (k, v) <- Aeson.KeyMap.toList o ]
```

```haskell
-- Figment.Provider.Dhall
import Control.Exception (SomeException, try)
import qualified Data.Aeson   as Aeson
import qualified Data.Text    as Text
import qualified Data.Text.IO as TIO
import qualified Dhall
import qualified Dhall.JSON as Dhall (dhallToJSON)
import Figment.Provider.Json (fromAeson)

instance Provider DhallFile where
  tagProvider d (DhallFile path) = do
    -- One 'try' covers reading, parsing, type-checking and the JSON
    -- conversion. 'dhallToJSON' returns an 'Either' with its own error
    -- type, so its 'Left' is rethrown to unify every failure mode.
    outcome <- try $ do
      expr <- Dhall.inputExpr =<< TIO.readFile path
      case Dhall.dhallToJSON expr of
        Left  compileErr -> ioError (userError (show compileErr))
        Right json       -> pure json
    pure $ case outcome :: Either SomeException Aeson.Value of
      Left  e -> Left (SourceError path (Text.pack (show e)))
      Right j -> mkTagged d (SourceId ("dhall:" <> Text.pack path), DefaultProfile)
                           (fromAeson j)
```

Dhall expressions can themselves construct deeply nested values, so the
depth budget matters here: the provider takes `d` rather than hard-coding
`defaultDepth`. Failures are caught as `SomeException` because the caller
has no need to discriminate among parse, type-check, import-resolution,
and JSON-conversion errors; the *message* is preserved in `SourceError`'s
`Text` field (§2.15) without a constructor per upstream exception type
(Standard §0.7). Dhall adds no algebraic content to `⊕`, is not
structurally generic (its key set depends on evaluating the expression,
not on `Rep R`), and remains additive and opt-in.

This block has not been compiled (§10.1).

---

## 9. Module Architecture

Drawn along the hexagonal boundary the Standard requires before the first
feature module (§2.6, Part 6 checklist):

```
figment/
  src/
    Figment/
      Value.hs         -- Core: Value, Tagged, Key, Path, SourceId, Profile (§1)
      Merge.hs         -- Core: Bias, ArrayPolicy, mergeAlg, pick (§2, §5)
      Layer.hs         -- Core: Layer, runLayers (§7)
      Error.hs         -- Core: ConfigError, the closed vocabulary (§8.1)
      Check.hs         -- Core (type-level only): Structural Reachability (§6, §8.4)
      Provider.hs      -- Adapter boundary: class Provider; EnvPrefixed, CliArgs,
                       --   Defaults, FieldProvider (§8.2)
      Provider/
        Env.hs         -- Adapter: Provider EnvPrefixed
        Cli.hs         -- Adapter: Provider CliArgs
        Defaults.hs    -- Adapter: Provider (Defaults r)
        Toml.hs        -- Adapter: TomlFile
        Json.hs        -- Adapter: JsonFile; fromAeson
        Yaml.hs        -- Adapter: YamlFile
        Dhall.hs       -- Adapter, opt-in: DhallFile (§8.5)
      Figment.hs       -- Public façade: composeChecked, composeUnchecked, re-exports
```

`Value.hs`, `Merge.hs`, `Layer.hs`, and `Error.hs` import nothing from
`Provider*`; `Check.hs` imports only the four provider newtype
declarations in `Provider.hs`, never an adapter — dependency direction
points inward, at the core (§2.6). Each `Provider/*` module owns exactly
one adapter and one reason to change (§0.1: "reads TOML" and "reads YAML"
are two sentences, hence two modules, not one `Providers.hs` grab-bag; see
also §2.10's ban on technical-utility grouping). Every module has an
explicit export list (§2.5, enforced with `-Wmissing-export-lists`).
`Value.hs` exports `Key`, `Path`, `SourceId`, `Profile`, `Tagged`,
`Value` and their smart constructors, but not `UnsafeDepth` or `Tagged`'s
raw constructor, because the depth-bound invariant (§1.3) depends on
construction going through `mkTagged`.

---

## 10. Verification Plan

Every algebraic claim ships with a named property test in the same change
that introduces it (Standard §5.5):

| Claim | Property test |
|---|---|
| Lemma 1 (identity, `Obj`-shaped) | `prop_identity_obj`: for arbitrary `Obj`-shaped `Tagged`, `mergeAlg RightWins ap t emptyObj`'s *value* equals `t`'s value. |
| Lemma 1 necessity (no identity off `Obj`) | `prop_no_identity_scalar`: for arbitrary non-`Obj` `Tagged`, `mergeAlg RightWins ap t emptyObj ≠ t` whenever `t ≠ emptyObj`. |
| Lemma 2 (non-commutativity) | `prop_rightwins_not_commutative`: exhibits the `Num 1`/`Num 2` counterexample as a regression test, not merely a generator hit. |
| Lemma 3 (associativity under schema-uniformity) | `prop_merge_associative`, for each `(Bias, ArrayPolicy)`, against the two-phase `SchemaUniform` generator (one shape, three independent fillings). |
| Lemma 3 necessity | `prop_associativity_counterexample_exists`: an *unrestricted* `Arbitrary Tagged` generator must find at least one associativity failure — a test that never fails is itself a bug (Standard §2.9); absence of counterexamples would mean the necessity claim was wrong. |
| §4 Provenance theorem | `prop_provenance_path_correct`: for arbitrary nested `Tagged` trees and an arbitrary merge chain, every leaf's `path` equals its own root-to-leaf key sequence. |
| §1.3 depth bound | `prop_depth_exceeded_is_total`: `mkTagged` on adversarially deep synthetic `RawJSON` (depth `10×` the budget) returns `Left (DepthExceeded ...)` in bounded stack space, never a stack overflow (RT-01). |
| §1.3 `DepthExceeded` reports the caller's budget | `prop_depth_exceeded_reports_budget`: for any positive budget `b` and input nested deeper than `b`, the error carries exactly `b`. |
| §1.3 merge does not compound depth | `prop_merge_bounded_by_max_operand_depth`: for any two `mkTagged`-produced trees each within `defaultDepth`, `mergeAlg`'s result depth is the `max` of the two, never their sum, for chains up to length 100. |
| §6 Structural Reachability | Compile-time only: a `should-not-typecheck` suite (`tasty-golden` against `stderr`) asserting `RequireGenericProvider AppConfig '[TomlFile]` fails with the exact `TypeError` text of §6.2, and that adding `EnvPrefixed` or `FieldProvider CustomProvider` to `ps` makes the same call site compile. |
| §8.4 `FieldsOf`/`OptionalFields` | Golden compile tests: a record with a `Maybe` field, a `Defaults`-covered field, and, as a negative case, a sum-typed "record" — the last must produce the named `TypeError`, not an opaque kind-mismatch dump. |

**Regression tests from comparable prior art (Standard §5.7).** Before
implementing the depth-bounded tagger and the file-based providers, the
issue trackers of at least the Rust `figment` crate (this library's
namesake and closest prior art), `serde_json`/`serde_yaml`, and `aeson`
are reviewed for their historically hit bug classes; each gets a named
regression test before or alongside the first implementation:

- **RT-01 — unbounded-recursion stack exhaustion on adversarial input**
  (the class §1.3 exists to close): `prop_depth_exceeded_is_total` above.
- **RT-02 — shape-mismatch silent reassociation across mixed-type
  layers** (the class Lemma 3's necessity clause documents): already a
  named property (`prop_associativity_counterexample_exists`), plus a
  regression fixture pinned to the exact `Arr`/`Obj` counterexample of §3.
- **RT-03 — env-var prefix collision** (`APP_DB_URL` versus a literal
  field named `dbUrl`, differing only in an ambiguous separator
  convention): a fixture test in `Provider/Env.hs` pinning the exact
  separator/casing rule chosen, so a refactor cannot silently change it.
- **RT-04 — TOML/YAML numeric-precision loss on round-trip** (a
  documented bug class in config-merging libraries, from
  `Scientific`-versus-`Double` coercion at parse boundaries): a property
  checking that a round-trip through `mkTagged` → merge → `runExtract`
  preserves an arbitrary `Scientific` exactly.

**Benchmarks (Standard §5.6).** `Merge.hs` and `Value.hs`'s `mkTagged` are
on the per-config-load path; both get a `criterion` benchmark checked into
CI with a regression threshold before either is merged (Part 6's setup
checklist), covering a realistically large (thousands of keys, tens of
levels) synthetic config as the large-input case Standard §4.10 requires
alongside the correctness test.

### 10.1 Verification status

Run on GHC 9.4.7 with `aeson-2.1.2.1`, `containers-0.6.7`, `text-2.0.2`,
`scientific-0.3.7.0`, `vector-0.13.1.0`, and `QuickCheck-2.14.3`. Stated
plainly so that no claim in this study outruns its evidence (Standard
§2.8):

| Claim | Evidence |
|---|---|
| Code in §1.2–§1.3, §5, §6.2, §7, §8.1–§8.4 typechecks | **Compiled.** |
| `fromAeson` (§8.5) typechecks against aeson-2.1 `Key`/`KeyMap` | **Compiled.** |
| Lemma 3 (associativity, schema-uniform) for all four `(Bias, ArrayPolicy)` pairs | **QuickCheck**, 100 cases each, schema-uniform generator: pass. |
| Lemma 1 (identity, `Obj`-shaped, value level) | **QuickCheck**: pass. |
| Lemma 3 necessity (the `Arr`/`Obj`/`Arr` counterexample) | **Executed**: left association gives `Arr [2]`, right association `Arr [1,2]`, as stated. |
| `mkTagged` rejects depth 40 under a caller budget of 30 and reports **30** | **Executed**: pass. |
| `mkTagged` accepts depth 40 under the default budget (64); rejects depth 640 000 with `DepthExceeded` and no stack overflow | **Executed**: pass. |
| `mkDepth 0` returns `Left (ConfigurationError …)` | **Executed**: pass. |
| `RequireGenericProvider` accepts and rejects as claimed in §6 | **Compiled** both ways; rejection message observed. |
| §8.5 Dhall provider (`dhallToJSON`, `inputExpr`) | **Not compiled** — `dhall`/`dhall-json` were unavailable in the verification environment. Names and types were checked against the published Hackage documentation (`inputExpr :: Text -> IO (Expr Src Void)`; `dhallToJSON` returns `Either CompileError Value`), but this block rests on documentation rather than a compiler. Gate 2 compiles it first. |
| `runExtract`, the TOML/YAML/Env/CLI/JSON provider instances, field-level coverage check | **Not implemented** — signatures or building blocks only; Gate 2 work, not verified claims. |
| Remaining properties of the §10 table (provenance, merge-depth, `prop_depth_exceeded_reports_budget`, RT-02–RT-04), benchmarks, prior-art review | **Planned**, not yet run. |

---

## 11. Architecture and Risk

**Highest-risk engineering problems, in descending order:**

1. **Provenance versus `aeson` interop — open.** Threading `Tagged`
   through `FromJSON` decoding needs either a custom `FromJSON Tagged`
   bridge or a two-pass decode, and no such bridge is designed anywhere in
   this study. It is the largest single line item in any effort estimate
   and the place Gate 1 (§12) must be satisfied before committing further
   design effort. It is a soundness risk only in the sense that an
   under-specified bridge could silently drop provenance, not in the sense
   that the merge algebra is unsound.
2. **Merge-strategy discoverability.** The `Layer`/`runLayers` design
   removes the *mixing* hazard entirely, reducing this to an
   ergonomics/naming question.
3. **Depth-budget tuning.** `defaultDepth = 64` is a judgment call, not a
   proof; Gate 2(e) validates it empirically. A schema legitimately deeper
   than 64 levels uses the explicit override (`tagProvider someDepth`, or
   `loadConfigWith someDepth`, §8.3). Low risk — deeply nested
   hand-authored config is itself a schema smell.
4. **Provider-fragmentation choice.** Separate packages versus one package
   with optional flags per file format: a policy decision, not a technical
   risk.
5. **Error-message quality.** Type-family errors (§6) must name the
   actual cause. Run-time errors must never leak an implementation detail:
   `DepthExceeded` and `ConfigurationError` messages are generated from the
   live `Depth`/`Path` values, never from a literal duplicating them
   (Standard §0.6).

---

## 12. Feasibility Gates and Recommendation

**Gate 1 — provenance demand.** Confirm that real configuration bugs in
the target codebase(s) are attributable to *not knowing which layer set a
given value*.

**Gate 2 — technical spike**, against the claims of §1–§10:

(a) The schema-uniform and unrestricted associativity properties (§10),
    in both directions.
(b) Leaf-level provenance, including `path`, survives a full round-trip
    through `aeson`'s `FromJSON` on a nested record with an array field.
(c) The `Port` smart constructor composes with `ConfigError` (§8.1), per
    §8.3.
(d) `RequireGenericProvider` fires a legible, single-message compile
    error on a provider-less required field; `FieldsOf`/`OptionalFields`
    resolve on a record with a `Maybe` field, a defaulted field, and a
    sum-typed negative test; `FieldProvider`-wrapped custom providers are
    correctly recognized.
(e) `mkTagged` rejects adversarially deep synthetic input in bounded
    stack space (RT-01), and a realistic config of ordinary depth decodes
    with no measurable overhead from the depth counting, confirmed by the
    §10 benchmark.
(f) The Dhall provider (§8.5) compiles against current `dhall` and
    `dhall-json`, and `runExtract` is implemented.

**Contingency.** If Gate 2(d) fails, `Figment.Check` is demoted to an
opt-in module and the MVP ships with `composeUnchecked` only. If Gate 2(e)
shows the depth counting imposes non-trivial overhead on realistic
configs, the budget check is reduced to a bare `Int` decrement (already
the design in §1.3) and re-benchmarked; no algorithmic escalation is
anticipated, since the check is a single machine-word comparison per
recursive step.

**Recommendation.** A qualified GO, conditional on Gate 1 for the
provenance half of the feature set and on Gate 2(e) for the
recursion-bound guarantee; Structural Reachability (§6) stands on its own
merit regardless of Gate 1. Minimum viable scope: the merge algebra of
§1–§7 unconditionally, **with `mkTagged`'s depth bound as a non-negotiable
part of the MVP, not a follow-up hardening pass** (unbounded recursive
descent over untrusted input is a Standard §2.4 violation, not an optional
robustness nicety); `Figment.Error` (§8.1) from the first commit, since
every other module's type signature depends on it; `Figment.Check` (§6,
§8.4) scoped to Structural Reachability, with `composeUnchecked` shipped
as a first-class escape hatch; Dhall support as an additive, opt-in
module.

---

## 13. Design Conformance Checklist

Items marked `[x]` are satisfied by the design as specified here; items
marked `[ ]` are specified but not yet done (see §10.1).

- [x] Single responsibility per module (§9): `Value`/`Merge`/`Layer`/
      `Error`/`Check` each own one concern; each `Provider/*` owns one
      adapter.
- [x] Extension axis identified per §0.2/§2.7: providers are
      typeclass-open (`class Provider`, third-party-extensible via
      `FieldProvider`); the four merge strategies are closed-sum and
      exhaustive (`Bias` × `ArrayPolicy`, owned and finite).
- [x] No lawless instances (§0.3): associativity holds only under
      schema-uniformity, so no `Semigroup`/`Monoid` instance is offered
      (§7); the hypothesis is enforced by the generator used to test it
      (§10).
- [x] Constraints are narrowest sufficient: `mergeAlg` needs no
      `Eq ArrayPolicy` (§5); `composeChecked` carries exactly
      `Generic r`, `FromJSON r`, and the reachability constraint.
- [x] Provider dependency inverted via `class Provider` (§0.5/§0.2); core
      modules do not import adapters (§9).
- [x] No duplicated invariant logic: `Elem` (§8.4) reuses the
      repeated-variable technique of §6.2.
- [x] No speculative generality: the `Depth` override (§8.2) exists
      because Gate 2(e) names it as a real, current requirement.
- [x] No structure reached through more than one level (§0.8): `Path` and
      `SourceId` are handed to `ConfigError` constructors directly, not
      reached through a `Tagged` value's nested `tMeta.path`.
- [x] No partial functions: `mkTagged`, `mkDepth`, `mkPort` return
      `Either ConfigError a`; `mergeAlg`'s pattern match is exhaustive
      (§5).
- [x] Illegal states unrepresentable: `Depth` cannot be non-positive
      (§1.3); `Path`/`Key`/`SourceId` cannot be confused with each other
      or with arbitrary `Text` (§1.2).
- [x] Cross-field invariants enforced once, at construction: the depth
      bound is enforced only inside `mkTagged`; nothing downstream
      re-checks it.
- [x] Closed domains are sum types: `Bias`, `ArrayPolicy`, `Profile`,
      `ConfigError`.
- [x] Errors structured and layered by pipeline stage: `ConfigError`
      (§8.1), one constructor per stage, matching §2.3.
- [x] `IO` confined to `Provider` instances; `Value`/`Merge`/`Layer` are
      pure and unit-testable without a shell.
- [x] Newtypes where concepts must not be confused (`Key`, `SourceId`,
      `Path`, `Depth`, `Port`); no GADT or existential is needed anywhere,
      and none is introduced speculatively (§0.7).
- [x] Single canonical IR: `Tagged`/`Value` is the one representation
      every provider funnels into and `runExtract` funnels out of (§2.1).
- [x] Recursive structures have an explicit, uniformly applied bound:
      `mkTagged`'s `Depth`, applied identically by every provider (§1.3).
- [x] Module exports only what is safe: `Value.hs` withholds `Tagged`'s
      raw constructor and `UnsafeDepth` (§9).
- [x] No hand-rolled recursion without a named shape: `mkTagged` is
      `traverse` over each container's children plus one necessarily
      hand-written structural recursion (no existing combinator threads a
      decreasing budget *and* builds a value), noted as such in its
      Haddock (§4.1's escape clause).
- [x] Named algorithms cited: `Data.Map.unionWith` (merge) and list
      concatenation (array `Concat`); no bespoke algorithm where a
      standard one suffices (§4.2, §4.7).
- [x] Functions are one lemma each, ≤ ~20 lines: `mkTagged`'s `go` is the
      borderline case; its Haddock states the single claim it proves.
- [x] Non-trivial comments state invariants, not procedures (§4.6).
- [x] Naming: `mkTagged`, `mkDepth`, `mkPort` follow the `mkX`
      smart-constructor convention (§4.8); `UnsafeDepth` and `UnsafePort`
      are prefixed `Unsafe` and unexported.
- [x] No strictness annotations beyond `foldl'` in `runLayers` (§7),
      which is justified by Standard §4.10's unbounded-chain concern.
- [x] No mutable local state.
- [x] Modules placed by domain (`Value`, `Merge`, `Provider/Env`), not
      `Utils` (§2.10).
- [x] Dependencies deliberate (§2.11): `scientific`, `aeson`, `vector`
      (`Vector.toList` in `fromAeson`, since aeson's `Array` is
      `Vector Value`), and, for the opt-in Dhall provider, `dhall` and
      `dhall-json` (`dhallToJSON`).
- [x] No `type` alias standing in for a `newtype` boundary (§3.12).
- [x] `Applicative` preferred where effects are independent: `mkPort <$>
      ... <*> ...` in §8.3's `loadConfigWith`.
- [x] Public boundary functions are concrete: `composeChecked` and
      `composeUnchecked` are concrete in `ConfigError`, and their
      polymorphism (`Generic r`, `FromJSON r`) is exactly what is
      exercised (§3.14, §0.4).
- [x] No dead code or stubs within what this study specifies: every
      function whose body is shown (`mkTagged`, `mkDepth`, `mkPort`,
      `mergeAlg`, `runLayers`, `fromAeson`, `composeChecked`/
      `composeUnchecked`, the §6.2/§8.4 type families) is fully defined
      and, per §10.1, compiled (the Dhall instance excepted).
- [x] Error messages name the specific thing and location: every
      `ConfigError` constructor carries a `Path` or `FilePath` (§8.1,
      §2.15), except `ConfigurationError`, which names the offending
      component and rule.
- [x] Depth-bounded recursion is stack-safe and tested on adversarial
      input (RT-01; §10.1, depth 640 000) (§4.10).
- [x] Space-leak hazards addressed by design: `runLayers` uses `foldl'`;
      `Arr` concatenation is bounded by the same depth discipline as
      everything else; providers parse strictly and hold no lazy
      `ByteString`/`Text` beyond the parse (§4.11).
- [x] Deviations recorded: the one judgment call made without a proof
      (`defaultDepth = 64`) is flagged as a risk item (§11, item 3) and a
      gate condition (§12, Gate 2(e)), per Standard §5.9.
- [ ] Every claimed invariant has a matching property test (§5.5):
      specified in the §10 table; Lemmas 1 and 3, Lemma 3's necessity,
      and the depth bound have been run (§10.1); the rest are to be
      implemented.
- [ ] Tests cover boundary, empty, maximum, and adversarial cases:
      RT-01 through RT-04 and the §10 table, per §10.1.
- [ ] Benchmarks checked in for `mkTagged` and `mergeAlg` (§10,
      Standard §5.6).
- [ ] Prior-art bug classes reviewed and pre-empted (§10's RT list,
      Standard §5.7).
