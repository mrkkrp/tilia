module Survey.Answer where

import Survey.Question
  ( questionText,
    questionId, -- unique within a survey, not across surveys
    -- answered before the respondent may go on
    questionRequired
  )

ask :: Question -> String
ask = questionText
