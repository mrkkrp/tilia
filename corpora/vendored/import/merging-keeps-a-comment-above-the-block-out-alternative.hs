module Telemetry.Report where

-- both halves of a sink live in the one module
import Telemetry.Sink (sinkFlush, sinkName)

describe :: Sink -> String
describe = sinkName
