module Survey.Render where

import Survey.Form
  ( Form
      ( formLocale,
        formPages, -- in the order they are shown
        formTitle
      ),
  )

title :: Form -> String
title = formTitle
