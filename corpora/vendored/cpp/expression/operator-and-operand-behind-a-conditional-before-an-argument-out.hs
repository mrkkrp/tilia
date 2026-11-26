{-# LANGUAGE CPP #-}

module Editor.Options where

editors =
  choose
    defaultEditor
#ifdef WITH_YI
    ++ yiEditor
      id
#else
    id
#endif

bracketed =
  choose
    (defaultEditor)
#ifdef WITH_YI
    ++ yiEditor
      id
#else
    id
#endif

listed =
  choose
    ["GtkSourceView"]
#ifdef WITH_YI
    ++ ["Yi"]
      id
#else
    id
#endif

appended =
  choose
    defaultEditor
#ifdef WITH_YI
    ++ [yiEditor]
      id
#else
    id
#endif
