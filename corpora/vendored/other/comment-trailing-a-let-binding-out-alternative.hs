module Route.Delay where

delay :: Int -> Int
delay minutes =
  let slack = 4 -- padding the timetable already allows
      late = minutes - slack
   in max 0 late
