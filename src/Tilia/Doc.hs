-- | Turning a printed document into source text.
module Tilia.Doc
  ( Doc,
    printDoc,
  )
where

import Data.Text (Text)
import Tilia.Doc.Internal
  ( Doc,
    render,
  )

-- | Render a document to source text.
printDoc :: Doc -> Text
printDoc = render
