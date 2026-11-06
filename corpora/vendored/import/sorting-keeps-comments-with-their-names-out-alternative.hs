module Survey.Answer where

import Survey.Question
  ( questionId, -- unique within a survey, not across surveys
    -- answered before the respondent may go on
    questionRequired,
    questionText,
  )

ask :: Question -> String
ask = questionText
