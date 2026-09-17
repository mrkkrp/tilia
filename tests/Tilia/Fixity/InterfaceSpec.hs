{-# LANGUAGE OverloadedStrings #-}

-- | Reading what @ghc --show-iface@ prints.
--
-- The samples below are cut from real output rather than invented, since
-- the whole risk here is in the format: this is a pretty-printer's idea of
-- an interface, not a documented one.
module Tilia.Fixity.InterfaceSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Test.Hspec
import Tilia.Fixity
import Tilia.Fixity.Interface

spec :: Spec
spec = do
  describe "what a module declares" $ do
    it "reads a fixity line" $
      declares "fixities infixl 9 !, infixl 9 !?, infixl 9 \\\\\n"
        `shouldBe` [ (OpName "!", Fixity LeftAssoc 9),
                     (OpName "!?", Fixity LeftAssoc 9),
                     (OpName "\\\\", Fixity LeftAssoc 9)
                   ]

    it "reads one wrapped across lines" $
      declares
        "fixities infixr 0 $, infixr 0 $!, infixl 4 *>, infixr 5 ++,\n\
        \         infixr 9 ., infixr 5 :|, infixl 4 <$\n"
        `shouldBe` [ (OpName "$", Fixity RightAssoc 0),
                     (OpName "$!", Fixity RightAssoc 0),
                     (OpName "*>", Fixity LeftAssoc 4),
                     (OpName "++", Fixity RightAssoc 5),
                     (OpName ".", Fixity RightAssoc 9),
                     (OpName ":|", Fixity RightAssoc 5),
                     (OpName "<$", Fixity LeftAssoc 4)
                   ]

    it "reads every direction, and a name used in backticks" $
      declares "fixities infixl 7 div, infix 4 ===, infixr 1 .&&.\n"
        `shouldBe` [ (OpName ".&&.", Fixity RightAssoc 1),
                     (OpName "===", Fixity NoAssoc 4),
                     (OpName "div", Fixity LeftAssoc 7)
                   ]

    it "reads the precedence GHC gives the function arrow" $
      declares "fixities infixr -1 ->\n"
        `shouldBe` [(OpName "->", Fixity RightAssoc (-1))]

    it "reads it alongside ordinary ones" $
      declares "fixities infixr -1 ->, infixl 9 !\n"
        `shouldBe` [(OpName "!", Fixity LeftAssoc 9), (OpName "->", Fixity RightAssoc (-1))]

    it "passes over an entry it cannot read, and keeps the rest" $
      declares "fixities infixl notadigit ?, infixl 9 !, infixl\n"
        `shouldBe` [(OpName "!", Fixity LeftAssoc 9)]

    it "says nothing for a module that declares nothing" $
      declares "exports:\n  member\n" `shouldBe` []

  describe "what a module passes on" $ do
    it "names the module an operator was declared in" $
      passesOn "exports:\n  Data.Aeson.Types.FromJSON..:\n"
        `shouldBe` [("Data.Aeson.Types.FromJSON", OpName ".:")]

    it "leaves out what the module declared itself" $
      passesOn "exports:\n  decode'\n  <+>\n" `shouldBe` []

    it "takes the members of a class along with it" $
      passesOn
        "exports:\n\
        \  Data.Aeson.Types.FromJSON.FromJSON{Data.Aeson.Types.FromJSON.parseJSON}\n"
        `shouldBe` [ ("Data.Aeson.Types.FromJSON", OpName "FromJSON"),
                     ("Data.Aeson.Types.FromJSON", OpName "parseJSON")
                   ]

    it "takes a record field, written after a bar" $
      passesOn "exports:\n  Data.Aeson.Encoding.Internal.Encoding'|{Data.Aeson.Encoding.Internal.fromEncoding}\n"
        `shouldBe` [ ("Data.Aeson.Encoding.Internal", OpName "Encoding'"),
                     ("Data.Aeson.Encoding.Internal", OpName "fromEncoding")
                   ]

    it "keeps a type apart from the module holding it" $
      passesOn "exports:\n  Data.Aeson.Types.Internal.Value\n"
        `shouldBe` [("Data.Aeson.Types.Internal", OpName "Value")]

    it "reads an operator that is nothing but a dot" $
      passesOn "exports:\n  Data.Function..\n"
        `shouldBe` [("Data.Function", OpName ".")]

    it "leaves a capitalised name this module declared alone" $
      passesOn "exports:\n  Value\n" `shouldBe` []

  describe "which namespace a fixity governs" $ do
    it "gives one to types where the module declares a type of that name" $
      declaresIn "fixities infix 4 :~:\nab12\n  data (:~:) a b where\n"
        `shouldBe` [((InTypes, OpName ":~:"), Fixity NoAssoc 4)]

    it "gives one to terms where nothing declares a type of that name" $
      declaresIn "fixities infixl 9 !\nab12\n  (!) :: Int -> Int -> Int\n"
        `shouldBe` [((InTerms, OpName "!"), Fixity LeftAssoc 9)]

    it "reads a type synonym as a type" $
      declaresIn "fixities infixr 5 :+\nab12\n  type (:+) :: * -> * -> *\n"
        `shouldBe` [((InTypes, OpName ":+"), Fixity RightAssoc 5)]

    it "reads a type family as a type" $
      declaresIn "fixities infixl 6 ==\nab12\n  type family (==) a b where\n"
        `shouldBe` [((InTypes, OpName "=="), Fixity LeftAssoc 6)]

    it "reads a class as a type" $
      declaresIn "fixities infixl 4 <%>\nab12\n  class (<%>) a where\n"
        `shouldBe` [((InTypes, OpName "<%>"), Fixity LeftAssoc 4)]

    it "takes a role declaration as saying the name is a type" $
      declaresIn "fixities infixl 9 !\nab12\n  type role (!) nominal\n"
        `shouldBe` [((InTypes, OpName "!"), Fixity LeftAssoc 9)]

    it "is not misled by declarations of other names" $
      declaresIn "fixities infixl 9 !\nab12\n  data Other a b where\n  (!) :: Int\n"
        `shouldBe` [((InTerms, OpName "!"), Fixity LeftAssoc 9)]

  describe "what a name carries with it" $ do
    it "takes the members an entry wears in braces" $
      carries "exports:\n  GHC.Internal.Base.NonEmpty{GHC.Internal.Base.:|}\n"
        `shouldBe` [(OpName "NonEmpty", [OpName ":|"])]

    it "takes every one of them" $
      carries
        "exports:\n\
        \  GHC.Internal.Base.Applicative{GHC.Internal.Base.*> GHC.Internal.Base.<*> GHC.Internal.Base.pure}\n"
        `shouldBe` [(OpName "Applicative", [OpName "*>", OpName "<*>", OpName "pure"])]

    it "takes them from a partial export, which still says what it has" $
      carries "exports:\n  GHC.Internal.Base.Functor|{GHC.Internal.Base.<$}\n"
        `shouldBe` [(OpName "Functor", [OpName "<$"])]

    it "takes a name this module declared, written without a module" $
      carries "exports:\n  WrappedArrow{WrapArrow unwrapArrow}\n"
        `shouldBe` [(OpName "WrappedArrow", [OpName "WrapArrow", OpName "unwrapArrow"])]

    it "keeps entries apart where several sit on one line" $
      carries "exports:\n  A{B} C{D}\n"
        `shouldBe` [(OpName "A", [OpName "B"]), (OpName "C", [OpName "D"])]

    it "has nothing to say about a name that carries nothing" $
      carries "exports:\n  decode'\n  Data.Aeson.Types.FromJSON..:\n" `shouldBe` []

  describe "sections it has no use for" $
    it "is not confused by the rest of the file" $ do
      let out =
            "Magic: Wanted 33214052,\n\
            \       got    33214052\n\
            \interface Data.Aeson 9103\n\
            \  interface hash: 6b4f\n\
            \exports:\n\
            \  Data.Aeson.Types.FromJSON..:\n\
            \fixities infixl 9 !\n\
            \direct package dependencies: base-4.20.2.0 bytestring-0.12.2.0\n\
            \orphans: Data.Orphans\n\
            \trusted: none\n"
      fmap (Map.toList . interfaceDeclares) (parseInterface "Data.Aeson" out)
        `shouldBe` Just [((InTerms, OpName "!"), Fixity LeftAssoc 9)]
      fmap interfaceReexports (parseInterface "Data.Aeson" out)
        `shouldBe` Just [("Data.Aeson.Types.FromJSON", OpName ".:")]

  describe "output it will not read" $ do
    it "refuses what does not name a module at all" $
      parseInterface "M" "some future rendering we do not recognise\n"
        `shouldBe` Nothing

    it "refuses an interface for a different module" $
      parseInterface "Data.Map.Strict" (header "Data.Map.Lazy" <> "fixities infixl 9 !\n")
        `shouldBe` Nothing

    it "reads one that names the module asked for" $
      declares "fixities infixl 9 !\n" `shouldBe` [(OpName "!", Fixity LeftAssoc 9)]

header :: Text -> Text
header modName = "interface " <> modName <> " 9103\n"

-- | The fixities an interface of this shape declares, by name alone.
declares :: Text -> [(OpName, Fixity)]
declares =
  fmap (\((_, op), fixity) -> (op, fixity))
    . maybe [] (Map.toList . interfaceDeclares)
    . parseInterface "M"
    . (header "M" <>)

-- | The same, keeping the namespace each governs.
declaresIn :: Text -> [((Namespace, OpName), Fixity)]
declaresIn =
  maybe [] (Map.toList . interfaceDeclares) . parseInterface "M" . (header "M" <>)

passesOn :: Text -> [(Text, OpName)]
passesOn = maybe [] interfaceReexports . parseInterface "M" . (header "M" <>)

-- | What each exported name carries with it, in a settled order.
carries :: Text -> [(OpName, [OpName])]
carries =
  maybe [] (fmap (fmap Set.toList) . Map.toList . interfaceChildren)
    . parseInterface "M"
    . (header "M" <>)
