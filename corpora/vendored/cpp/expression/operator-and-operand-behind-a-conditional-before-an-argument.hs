{-# LANGUAGE CPP #-}

module Editor.Options where

editors =
  choose
    defaultEditor
#ifdef WITH_YI
    ++ yiEditor
#endif
    id

bracketed =
  choose
    (defaultEditor)
#ifdef WITH_YI
    ++ yiEditor
#endif
    id

listed =
  choose
    ["GtkSourceView"]
#ifdef WITH_YI
    ++ ["Yi"]
#endif
    id

appended =
  choose
    defaultEditor
#ifdef WITH_YI
    ++ [yiEditor]
#endif
    id
