{-# LANGUAGE CPP #-}

module Editor.Options where

editors =
    comboSelectionEditor
        ["GtkSourceView"]
#ifdef WITH_YI
        ++ ["Yi"]
#endif
#ifdef WITH_CODE_MIRROR
        ++ ["CodeMirror"]
#endif
            id
