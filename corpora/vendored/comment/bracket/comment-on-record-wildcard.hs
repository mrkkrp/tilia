{-# LANGUAGE RecordWildCards #-}

wildcardAlone =
  Shape
    { .. -- whatever is in scope
    }

wildcardLast =
  Shape
    { name = "circle",
      colour = Red,
      .. -- and the radius comes from above
    }

wildcardWithARemarkAbove =
  Shape
    { name = "square",
      -- the sides are already bound
      .. -- so they need no mention
    }

wildcardInAPattern Shape {..} = name -- bound by the wildcard

wildcardInAPatternWithFields
  Shape
    { name, -- named outright
      ..
    } = name
