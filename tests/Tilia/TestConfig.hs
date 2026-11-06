{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- | How every corpus example is formatted.
module Tilia.TestConfig
  ( formatExample,
    configurationsToCheck,
    defaultsFor,
    effectiveExtensionsFor,
  )
where

import Data.Choice (Choice, fromBool, isTrue)
import Data.List (maximumBy, nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Text (Text)
import GHC.Driver.Session qualified as GHC
import GHC.LanguageExtensions.Type (Extension (ImportQualifiedPost))

import Tilia.BootFixities (bootFixities)
import Tilia.Cabal.Project (Marker (ProjectFile), ProjectRoot (..))
import Tilia.Fixity
  ( Direction (..),
    Established (..),
    Fixities,
    Fixity (..),
    Namespace (..),
    OpName (..),
  )
import Tilia.Fixity.Plan (Resolver (..))
import Tilia.Format (AstCheck (..), FormatError, Session (..), formatSource)
import Tilia.Pragma (Defaults, defaultsOf, effectiveExtensions)
import Tilia.TestUtils (inBothNamespaces)

-- | Format a corpus example with 'formatSource', in a project where only
-- the boot packages can be read.
formatExample ::
  -- | Whether to check idempotence and the AST, in every configuration.
  Choice "check" ->
  -- | What the package around it puts in force, if it is a module of one.
  Maybe Defaults ->
  -- | The modules the package around it declares, each with the package's
  -- name.
  Map Text (Set Text) ->
  -- | Its name in the corpus.
  FilePath ->
  -- | The example.
  Text ->
  IO (Either FormatError Text)
formatExample checking package ownModules =
  formatSource
    Session
      { sessionRoot = ProjectRoot "." ProjectFile,
        sessionOwnModules = ownModules,
        sessionResolver =
          Resolver $ \_ name ->
            pure mempty{establishedFixities = exportsOf name},
        sessionMacros = mempty,
        sessionPackage = \_ -> pure (Right (defaultsFor package)),
        sessionCheckAst =
          if isTrue checking
            then EachCppConfiguration configurationsToCheck
            else NoAstCheck,
        sessionCheckIdempotence = fromBool (isTrue checking),
        sessionFixityNotes = Nothing,
        sessionUnsolved = Nothing
      }

-- | How many configurations one example gets compared over.
configurationsToCheck :: Int
configurationsToCheck = 64

-- | What a corpus example's package puts in force, or 'Nothing' for an
-- example of no package.
--
-- An example of no package is read as GHC2021 without
-- @ImportQualifiedPost@, so that @import qualified@ is printed the same
-- under every edition.
defaultsFor ::
  -- | What the package around it puts in force, if it is a module of one.
  Maybe Defaults ->
  Defaults
defaultsFor = fromMaybe (defaultsOf edition [])
  where
    edition = filter (/= ImportQualifiedPost) ghc2021
    ghc2021 = GHC.languageExtensions (Just GHC.GHC2021)

-- | The extensions in force in a corpus example, given what its package
-- puts in force.
effectiveExtensionsFor ::
  -- | What the package around it puts in force, if it is a module of one.
  Maybe Defaults ->
  -- | The example.
  Text ->
  [Extension]
effectiveExtensionsFor = effectiveExtensions . defaultsFor

-- | What a module in scope exports, as far as the corpus is concerned: a
-- boot module its own operators, any other every operator, standing for
-- what it may re-export, and every module the ones from 'elsewhere'.
exportsOf ::
  -- | The module's name.
  Text ->
  Fixities
exportsOf name = case Map.lookup name bootFixities of
  Just exact ->
    Map.restrictKeys agreed (Map.keysSet exact <> Map.keysSet outside)
  Nothing -> agreed
  where
    outside = inBothNamespaces elsewhere

-- | One fixity for every operator, so that no two imports of an example
-- disagree on one.
--
-- Where the boot packages give an operator different fixities, the one
-- most of the modules exporting it give is taken, unless 'preferred' names
-- another, and a boot package's is taken over one from 'elsewhere'.
agreed :: Fixities
agreed =
  Map.unions [preferred, Map.map mostCommon offered, inBothNamespaces elsewhere]
  where
    offered = Map.unionsWith (<>) (fmap (fmap pure) (Map.elems bootFixities))
    mostCommon fixities =
      snd . maximumBy (comparing fst) $
        [(length (filter (== f) fixities), f) | f <- nub fixities]

-- | The fixities the examples mean where most of the boot modules
-- exporting an operator give another.
preferred :: Fixities
preferred =
  Map.fromList
    [ ((InTerms, OpName op), fixity)
    | (op, fixity) <-
        [ -- base's, over parsec's
          ("<|>", Fixity LeftAssoc 3),
          -- base's, over pretty's and template-haskell's
          ("<>", Fixity RightAssoc 6),
          -- Data.List's, over containers', which is mostly written
          -- qualified
          ("\\\\", Fixity NoAssoc 5),
          -- ReadP's, over ReadPrec's, which declares none
          ("+++", Fixity RightAssoc 5),
          ("<++", Fixity RightAssoc 5)
        ]
    ]

-- | Operators the examples use that no boot package exports.
--
-- The same list Ormolu's test suite carries, for the same reason: these
-- turn up in the examples, their fixities are not discoverable from
-- anything to hand, and without them those examples are laid out as though
-- every one of these were @infixl 9@.
elsewhere :: Map OpName Fixity
elsewhere =
  Map.fromList $
    concat
      [ ormoluOverrides,
        lens,
        esqueleto,
        servant,
        hspec,
        preludeInfix,
        outsideBoot
      ]
  where
    infixL p ops = [(OpName o, Fixity LeftAssoc p) | o <- ops]
    infixR p ops = [(OpName o, Fixity RightAssoc p) | o <- ops]
    infixN p ops = [(OpName o, Fixity NoAssoc p) | o <- ops]

    -- Five operators the corpus uses that belong to no package we can
    -- consult, and whose fixities are therefore whatever the corpus was laid
    -- out with. Two disagree with the library the spelling comes from—@.=@
    -- is @infix 4@ in lens and @#@ is @infixr 8@ there—but the expected
    -- outputs settle it, since matching them is the only thing these are
    -- for.
    ormoluOverrides =
      infixR 8 [".="]
        <> infixR 5 ["#"]
        -- Ormolu gives these 3, 3.3 and 3.7, which it can because its
        -- precedences are fractional and ours are whole numbers. Only their
        -- order relative to one another is ever exercised, and that is kept.
        <> infixR 3 [">~<"]
        <> infixR 4 ["|~|"]
        <> infixR 5 ["<~>"]

    -- @lens@, and the packages that copy its spelling.
    lens =
      infixL 8 ["^.", "^..", "^?", "^?!", "^@.", "^@..", "^@?"]
        <> infixR
          4
          [ ".~",
            "%~",
            "?~",
            "+~",
            "-~",
            "*~",
            "//~",
            "^~",
            "^^~",
            "**~",
            "||~",
            "&&~",
            "<>~",
            "<.~",
            "<?~"
          ]
        <> infixN 4 ["%=", "?=", "+=", "-=", "*=", "//=", "<>=", ".~=", "%%="]
        <> infixR 9 ["<.", ".>", "<.>"]

    -- @esqueleto@, whose comparisons are the SQL ones with a dot on the end.
    esqueleto =
      infixL 9 ["?."]
        <> infixN 4 ["==.", "!=.", ">=.", ">.", "<=.", "like", "%."]
        <> infixR 3 ["&&."]
        <> infixR 2 ["||."]
        <> infixL 6 ["+.", "-."]
        <> infixL 7 ["*.", "/."]
        <> infixL 2 [":&"]

    -- @servant@'s way of spelling an API.
    servant = infixR 4 [":>"] <> infixR 3 [":<|>"]

    -- @hspec@ writes its expectations infix, and they are meant to be the
    -- loosest thing on the line.
    hspec =
      infixN
        1
        [ "shouldBe",
          "shouldNotBe",
          "shouldSatisfy",
          "shouldNotSatisfy",
          "shouldContain",
          "shouldNotContain",
          "shouldMatchList",
          "shouldReturn",
          "shouldNotReturn",
          "shouldThrow",
          "shouldStartWith",
          "shouldEndWith"
        ]

    -- Functions written infix often enough to be worth knowing the fixity
    -- of, and which the list of module exports does not carry.
    preludeInfix = infixR 0 ["seq"] <> infixL 0 ["on"]

    -- Loosest-binding operators from packages outside the boot libraries.
    -- These decide layout rather than grouping: an @infixr 0@ is written at
    -- the end of the line it breaks, the way @$@ is, and without the fixity
    -- the examples come out with the operator at the start of the next one.
    outsideBoot = infixR 0 ["deepseq", "?:"]
