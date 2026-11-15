module Telemetry.Report where

-- both halves of a sink live in the one module
import Telemetry.Sink (sinkName)
import Telemetry.Sink (sinkFlush)

describe :: Sink -> String
describe = sinkName
