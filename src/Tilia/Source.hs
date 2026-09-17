-- | The module as its author wrote it.
module Tilia.Source
  ( -- * The source
    SourceType (..),
    Written (..),
    Source,
    sourceOf,

    -- * Its lines
    Lines,
    linesOf,
    dropping,
    lineTexts,
    sourceLines,
    lineAt,
    blankAt,
    blankBelow,
    closesABranch,
    directivePresentOnLine,
    directiveOnLine,

    -- * Its comments
    comments,
  )
where

import GHC.Hs (HsModule)
import GHC.Hs.Extension (GhcPs)
import GHC.Parser.Annotation (LEpaComment)
import Tilia.Comments (Comment, commentsOf)
import Tilia.Source.Lines

-- | Whether a file is a module or a Backpack signature.
data SourceType
  = ModuleSource
  | SignatureSource
  deriving (Eq, Show)

-- | A module's source in a form that facilitates querying.
data Source = Source
  { -- | The lines, numbered from one as the compiler numbers them.
    srcLines :: !Lines,
    -- | Every comment in the module, in source order.
    srcComments :: [Comment]
  }

-- | The lines of a source.
sourceLines :: Source -> Lines
sourceLines = srcLines

-- | Read a module's source.
sourceOf ::
  -- | The lines of the module, as this configuration has them.
  Lines ->
  -- | Comments the syntax tree does not carry. See 'commentsOf'.
  [LEpaComment] ->
  -- | The result of parsing.
  HsModule GhcPs ->
  Source
sourceOf ls loose hsModule =
  Source
    { srcLines = ls,
      srcComments = commentsOf ls loose hsModule
    }

-- | Every comment in a module, in source order.
comments :: Source -> [Comment]
comments = srcComments
