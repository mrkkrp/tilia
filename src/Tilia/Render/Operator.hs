{-# LANGUAGE LambdaCase #-}

-- | Regrouping a chain of infix operators by precedence.
module Tilia.Render.Operator
  ( OpChain (..),
    flatten,
    flattenAround,
    associate,
    chainSpan,
    lastOperand,
    isSeparator,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isNothing, mapMaybe)
import Tilia.Fixity (Direction (..), Fixity (..))
import Tilia.Span

-- | A chain of operator applications.
--
-- A branch holds @n + 1@ operands and @n@ operators, all of which bind
-- equally tightly. This is the shape layout wants: the operators of one
-- level are siblings, so the printer can decide once how that level breaks
-- rather than rediscovering it at every binary node.
data OpChain a op
  = Operand a
  | Chain (NonEmpty (OpChain a op)) [op]
  deriving (Eq, Show)

-- | Take a binary application tree apart into one flat run.
--
-- The decomposition function returns the two operands and the operator of a
-- node that is an application of an infix operator, and nothing for a node
-- that is a leaf.
flatten ::
  -- | Take one node apart, if it comes apart.
  (a -> Maybe (a, op, a)) ->
  -- | The root of the tree.
  a ->
  (NonEmpty a, [op])
flatten split = go
  where
    go x = case split x of
      Nothing -> (x :| [], [])
      Just (l, op, r) ->
        let (ls, lops) = go l
            (rs, rops) = go r
         in (ls <> rs, lops <> [op] <> rops)

-- | 'flatten' for a node the caller has already taken apart.
--
-- The printers match on the operator application in order to reach its
-- parts, so by the time a chain is being built the outermost node has
-- already been destructured and there is nothing left to hand to 'flatten'.
flattenAround ::
  -- | Take one node apart, if it comes apart.
  (a -> Maybe (a, op, a)) ->
  -- | The left operand of the node already taken apart.
  a ->
  -- | Its operator.
  op ->
  -- | Its right operand.
  a ->
  (NonEmpty a, [op])
flattenAround split l op r =
  let (ls, lops) = flatten split l
      (rs, rops) = flatten split r
   in (ls <> rs, lops <> (op : rops))

-- | Regroup a flat run by precedence.
--
-- The loosest-binding operators of the run become the operators of the top
-- branch, and everything between two of them becomes a subtree, regrouped
-- the same way. When any operator in the run has no known precedence the
-- run is left as one flat branch: nothing is asserted about how it
-- associates, so nothing is rearranged.
associate ::
  -- | The fixity of an operator, if it was established.
  (op -> Maybe Fixity) ->
  -- | The operands of the run, in the order written.
  NonEmpty a ->
  -- | The operators standing between them.
  [op] ->
  OpChain a op
associate fixityOf = build
  where
    build (x :| []) _ = Operand x
    build operands ops
      | any (isNothing . precedenceOf) ops = flatBranch operands ops
      | otherwise =
          case splitOn ((== Just loosest) . precedenceOf) operands ops of
            (groups, splitters) -> Chain (fmap (uncurry build) groups) splitters
      where
        loosest = minimum (mapMaybe precedenceOf ops)
    flatBranch operands ops = Chain (Operand <$> operands) ops
    precedenceOf = fmap fixityPrecedence . fixityOf

-- | Cut a run wherever the operator satisfies the predicate.
splitOn ::
  -- | Which operators cut the run.
  (op -> Bool) ->
  -- | The operands of the run, in the order written.
  NonEmpty a ->
  -- | The operators standing between them.
  [op] ->
  (NonEmpty (NonEmpty a, [op]), [op])
splitOn cuts (x0 :| xs) ops = go (x0 :| []) [] (zip ops xs)
  where
    go current currentOps [] = ((NE.reverse current, reverse currentOps) :| [], [])
    go current currentOps ((op, y) : rest)
      | cuts op =
          let (groups, splitters) = go (y :| []) [] rest
           in (NE.cons (NE.reverse current, reverse currentOps) groups, op : splitters)
      | otherwise = go (NE.cons y current) (op : currentOps) rest

-- | The region of the input a chain came from.
chainSpan :: (a -> Maybe Span) -> OpChain a op -> Maybe Span
chainSpan spanOfOperand = \case
  Operand x -> spanOfOperand x
  Chain xs _ -> foldr1 join' (chainSpan spanOfOperand <$> xs)
  where
    join' (Just a) (Just b) = Just (a <> b)
    join' a b = maybe b Just a

-- | The rightmost operand of a chain.
lastOperand :: OpChain a op -> a
lastOperand = \case
  Operand x -> x
  Chain xs _ -> lastOperand (NE.last xs)

-- | Is this operator one of the ones that exist to separate rather than to
-- combine?
isSeparator :: Maybe Fixity -> Bool
isSeparator = \case
  Just (Fixity RightAssoc 0) -> True
  _ -> False
