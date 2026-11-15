module Survey.Render where

import Survey.Form (Form (formTitle))
import Survey.Form
  ( Form
      ( formPages, -- in the order they are shown
        formLocale
      )
  )

title :: Form -> String
title = formTitle
