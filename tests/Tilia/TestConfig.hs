{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The settings every corpus example is formatted with.
module Tilia.TestConfig
  ( exampleRenderConfig,
  )
where

import Data.Choice (pattern Is)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Hs (HsModule)
import GHC.Hs.Extension (GhcPs)
import GHC.LanguageExtensions.Type (Extension)
import Tilia.Fixity
  ( Direction (..),
    Fixities,
    Fixity (..),
    KnownModules (..),
    Namespace (..),
    OpName (..),
    Provenance (..),
    Reach (..),
    Scope (..),
    inBothNamespaces,
    noKnownModules,
    operatorsUsed,
    resolveScope,
  )
import Tilia.Fixity.Builtin (builtinFixities)
import Tilia.Pragma (effectiveExtensions)
import Tilia.Render (RenderConfig (..), defaultRenderConfig)

-- | How to format one corpus example.
exampleRenderConfig ::
  -- | What the package around it puts in force, if it is a module of one.
  [Extension] ->
  Text ->
  HsModule GhcPs ->
  RenderConfig
exampleRenderConfig package source hsModule =
  defaultRenderConfig
    { rcExtensions =
        Set.fromList (effectiveExtensions package source),
      rcScope =
        Just
          ( underEveryQualifier
              (resolveScope (Is #implicitPrelude) known hsModule)
          )
    }
  where
    known = noKnownModules{knownFixities = exportsOf}
    underEveryQualifier scope =
      scope
        { scopeInTypes = alsoQualified (scopeInTypes scope),
          scopeInTerms = alsoQualified (scopeInTerms scope)
        }
    alsoQualified reach =
      reach
        { reachQualified =
            Map.union
              (reachQualified reach)
              ( Map.fromList
                  [ ((qualifier, op), (fixity, DeclaredIn qualifier))
                  | (_, (Just qualifier, op)) <- operatorsUsed hsModule,
                    Just exported <- [exportsOf qualifier],
                    Just fixity <- [Map.lookup (InTerms, op) exported]
                  ]
              )
        }

-- | What a module in scope exports, as far as the corpus is concerned.
exportsOf :: Text -> Maybe Fixities
exportsOf name = Just (Map.union ours (inBothNamespaces elsewhere))
  where
    ours = case Map.lookup name builtinFixities of
      Just exact -> exact
      Nothing -> everythingKnown

-- | Every operator any boot module exports.
everythingKnown :: Fixities
everythingKnown = Map.unions (Map.elems builtinFixities)

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
