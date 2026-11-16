{-# LANGUAGE TemplateHaskell #-}

module Query.Names where

import qualified Data.List.NonEmpty as NonEmpty
import qualified Query.Builder as Builder

consName :: Name
consName = '(:)

appendName :: Name
appendName = '(<>)

nonEmptyName :: Name
nonEmptyName = ''(NonEmpty.:|)

joinName :: Name
joinName = '(Builder.<+>)

eqName :: Name
eqName = ''(==)

unitName :: Name
unitName = ''()

listName :: Name
listName = ''[]

pairName :: Name
pairName = ''(,)

selectName :: Name
selectName = 'Builder.select

clauseName :: Name
clauseName = ''Builder.Clause
