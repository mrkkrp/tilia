-- Constructs nested in one another, which a narrow column limit breaks
-- level after level.
module Loom.Nesting where

woven =
  weave
    (selvedge (threadsOf loom) (edgeWidth loom))
    (twill [warpOf loom, weftOf loom] (Just (tensionOf loom)))

table =
  [(warpName loom, [threadColour thread | thread <- loomWarp loom]) | loom <- looms, isActive loom]

report loom = case loomState loom of
  Idle since -> describeIdle since (loomName loom)
  Busy job -> describeJob job

settings =
  Settings
    { loomSize = (loomWidth defaultLoom, loomDepth defaultLoom),
      loomDyes = [Indigo, Madder, Weld]
    }
