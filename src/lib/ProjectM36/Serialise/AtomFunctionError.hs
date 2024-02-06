{-# OPTIONS_GHC -fplugin=Graph.Trace #-}
{-# LANGUAGE DerivingVia, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ProjectM36.Serialise.AtomFunctionError where
import Codec.Winery
import Graph.Trace
import ProjectM36.AtomFunctionError

deriving via WineryVariant AtomFunctionError instance Serialise AtomFunctionError
