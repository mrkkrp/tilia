{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | The account a run gives of how it settled a module's operators.
module Tilia.Fixity.DebugSpec (spec) where

import Data.Choice (pattern Is)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec
import Tilia.Fixity
  ( Direction (..),
    Fixity (..),
    KnownModules (..),
    OpName (..),
    inBothNamespaces,
    noKnownModules,
    resolveScope,
  )
import Tilia.Fixity.Debug (fixityNotes, renderFixityNotes)
import Tilia.Palette (Palette (Plain))
import Tilia.Parser (defaultParserConfig, describeParseError, parseModule, pmModule)
import Tilia.Utils (lineWidth, visibleLength)

spec :: Spec
spec = do
  describe "what each import brought" $ do
    it "counts the operators a module was read for" $
      notesFor [("Prelude", Just []), ("Data.Map", Just [("!", infixl' 9)])] "import Data.Map\n"
        >>= (`shouldContain'` "· Data.Map: 1 operator")

    it "says so when a module could not be read" $
      notesFor [("Prelude", Just [])] "import Criterion.Main\n"
        >>= (`shouldContain'` "· Criterion.Main: could not be read")

    it "keeps the alias a qualified import goes under" $
      notesFor [("Prelude", Just []), ("Data.Map", Just [])] "import qualified Data.Map as M\n"
        >>= (`shouldContain'` "· Data.Map qualified as M: 0 operators")

    it "names the Prelude, which nobody wrote but everybody imports" $
      notesFor [("Prelude", Just [("+", infixl' 6)])] "f = 1\n"
        >>= (`shouldContain'` "· Prelude: 1 operator")

  describe "what became of each operator" $ do
    it "names the import that carried the fixity" $
      notesFor
        [("Prelude", Just []), ("Data.Map", Just [("!", infixl' 9)])]
        "import Data.Map\nf m = m ! 1\n"
        >>= (`shouldContain'` "· ! infixl 9, declared in Data.Map")

    it "says when the module declared it itself" $
      notesFor [("Prelude", Just [])] "infixr 5 <+>\nf a b = a <+> b\n"
        >>= (`shouldContain'` "· <+> infixr 5, declared in this module")

    it "says when nothing in scope declares it and everything was read" $
      notesFor [("Prelude", Just [])] "f a b = a <?> b\n"
        >>= (`mentions` "<?> infixl 9, the Report's default")

    it "says which unread module the answer might have been in" $
      notesFor [("Prelude", Just [])] "import Criterion.Main\nf a b = a <?> b\n"
        >>= ( `shouldContain'`
                "· <?> unknown: may be declared in Criterion.Main, which this run could not read"
            )

    it "counts the unread modules when there is more than one" $
      notesFor
        [("Prelude", Just [])]
        "import Criterion.Main\nimport Test.Tasty\nf a b = a <?> b\n"
        >>= ( `mentions`
                "may be declared in Criterion.Main or Test.Tasty, neither of which this run could read"
            )

    it "keeps the qualifier an operator was written under" $
      notesFor
        [("Prelude", Just []), ("Data.Map", Just [("!", infixl' 9)])]
        "import qualified Data.Map as M\nf m = m M.! 1\n"
        >>= (`shouldContain'` "· M.! infixl 9, declared in Data.Map")

    it "says when two imports disagree about one" $
      notesFor
        [ ("Prelude", Just []),
          ("Left", Just [("<+>", infixl' 6)]),
          ("Right", Just [("<+>", Fixity RightAssoc 5)])
        ]
        "import Left\nimport Right\nf a b = a <+> b\n"
        >>= (`mentions` "two modules in scope disagree about it")

    it "gives an operator one line however often it is written" $ do
      told <- notesFor [("Prelude", Just [])] "f a b c = a <?> b <?> c <?> a\n"
      length (filter (T.isInfixOf "<?>") told) `shouldBe` 1

  describe "how far reading an import got" $ do
    it "names the module that stopped it rather than the import above it" $
      throughHspec
        >>= ( `mentions`
                "may be declared in Test.Hspec → Test.Hspec.Core.Spec \
                \→ Test.QuickCheck.Property, which this run could not read"
            )

    it "says what an import that could not be read was reached through" $
      throughHspec
        >>= ( `mentions`
                "Test.Hspec: could not be read, through Test.Hspec.Core.Spec \
                \→ Test.QuickCheck.Property"
            )

    it "adds nothing for an import unread on its own account" $
      notesFor [("Prelude", Just [])] "import Criterion.Main\nf a b = a <?> b\n"
        >>= (`shouldContain'` "· Criterion.Main: could not be read")

  describe "the shape of it" $ do
    it "keeps every line it prints inside the width" $ do
      told <- throughHspec
      filter ((> lineWidth) . visibleLength) told `shouldBe` []

    it "sets a line it had to break further in than the entry it belongs to" $ do
      told <- throughHspec
      let indentOf = T.length . T.takeWhile (== ' ')
          opens l = "·" `T.isPrefixOf` T.stripStart l
      case break (T.isInfixOf "Test.QuickCheck.Property") told of
        (above, broken : _)
          | not (opens broken),
            (entry : _) <- filter opens (reverse above) ->
              indentOf broken `shouldSatisfy` (> indentOf entry)
        _ -> expectationFailure (show told)

    it "sets out under headings" $ do
      told <- notesFor [("Prelude", Just [("+", infixl' 6)])] "f a b = a + b\n"
      fmap T.stripStart told `shouldContain` ["· imports"]
      fmap T.stripStart told `shouldContain` ["· operators"]

    it "leaves out a heading it would have nothing to put under" $
      notesFor [("Prelude", Just [])] "f = 1\n"
        >>= (`shouldSatisfy` all ((/= "· operators") . T.stripStart))

    it "indents an entry further than the heading it sits under" $ do
      told <- notesFor [("Prelude", Just [])] "import Data.Map\n"
      let indentOf = T.length . T.takeWhile (== ' ')
          under heading = [indentOf l | l <- told, heading `T.isInfixOf` l]
      case (under "· imports", under "· Data.Map") of
        ([heading], [there]) -> there `shouldSatisfy` (> heading)
        (headings, entries) ->
          expectationFailure (show (headings, entries))

    it "says nothing about declarations a module does not make" $
      notesFor [("Prelude", Just [])] "f = 1\n"
        >>= (`shouldSatisfy` all (not . T.isInfixOf "declared here"))

    it "lists what the module declares for itself" $
      notesFor [("Prelude", Just [])] "infixr 5 <+>\nf a b = a <+> b\n"
        >>= (`shouldContain'` "· <+> infixr 5")

----------------------------------------------------------------------------
-- Helpers

-- | The account given of a module, against a world of imports that could be
-- read and imports that could not.
--
-- A module named in the world is readable and exports what is listed; a
-- module absent from it is one the resolver could not read at all.
notesFor :: [(Text, Maybe [(Text, Fixity)])] -> Text -> IO [Text]
notesFor = notesThrough []

-- | The same, told how far reading got below each import it could not read.
notesThrough ::
  -- | What lies below an import, ending at the module that stopped it.
  [(Text, [Text])] ->
  [(Text, Maybe [(Text, Fixity)])] ->
  Text ->
  IO [Text]
notesThrough chains world source =
  renderFixityNotes Plain . Map.singleton "M.hs"
    <$> fixityNotes (Is #implicitPrelude) (pure . exportsOf) chainOf scope hsModule
  where
    scope =
      resolveScope
        (Is #implicitPrelude)
        noKnownModules {knownFixities = exportsOf, knownChain = chainFor}
        hsModule
    chainFor m = maybe [] id (lookup m chains)
    chainOf = pure . chainFor
    hsModule = pmModule parsed
    parsed = case parseModule defaultParserConfig "M.hs" ("module M where\n" <> source) of
      Left problem -> error (T.unpack (describeParseError problem))
      Right m -> m
    exportsOf m = do
      declared <- lookup m world
      inBothNamespaces . Map.fromList . fmap (\(op, fixity) -> (OpName op, fixity))
        <$> declared

infixl' :: Int -> Fixity
infixl' = Fixity LeftAssoc

-- | A module whose one import could not be read, and whose reading stopped
-- two modules further down. The real shape, and long enough to have to be
-- broken to fit the width.
throughHspec :: IO [Text]
throughHspec =
  notesThrough
    [("Test.Hspec", ["Test.Hspec.Core.Spec", "Test.QuickCheck.Property"])]
    [("Prelude", Just [])]
    "import Test.Hspec\nf a b = a <?> b\n"

-- | Is this line among them, whatever it was indented by?
shouldContain' :: [Text] -> Text -> Expectation
shouldContain' told wanted =
  fmap T.stripStart (rejoined told) `shouldContain` [wanted]

-- | Does some line say this much, whatever else it goes on to say?
mentions :: [Text] -> Text -> Expectation
mentions told wanted = rejoined told `shouldSatisfy` any (T.isInfixOf wanted)

-- | The entries as they read before they were broken to fit the width.
rejoined :: [Text] -> [Text]
rejoined = foldl add []
  where
    add seen l
      | null seen || "·" `T.isPrefixOf` T.stripStart l = seen <> [l]
      | otherwise = case unsnoc seen of
          Just (earlier, one) -> earlier <> [one <> " " <> T.stripStart l]
          Nothing -> [l]
    unsnoc xs = case reverse xs of
      [] -> Nothing
      (x : rest) -> Just (reverse rest, x)
