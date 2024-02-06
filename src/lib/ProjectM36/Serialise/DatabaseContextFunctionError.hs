{-# OPTIONS_GHC -fplugin=Graph.Trace #-}
{-# LANGUAGE DerivingVia, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ProjectM36.Serialise.DatabaseContextFunctionError where
import Codec.Winery
import Graph.Trace
import ProjectM36.DatabaseContextFunctionError

deriving via WineryVariant DatabaseContextFunctionError instance Serialise DatabaseContextFunctionError
