{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- A promoted tuple or list whose first item is a character keeps a space
-- after its tick, or the tick and the bracket would read as a character.
module Key.Binding where

type family Shortcut key where
  Shortcut '( 'q', mods) = Quit
  Shortcut '( 's', mods) = Save
  Shortcut key = Ignore

type Arrows = '[ 'h', 'j', 'k', 'l']
