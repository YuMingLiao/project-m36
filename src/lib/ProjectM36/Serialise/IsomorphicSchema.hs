{-# OPTIONS_GHC -fplugin=Graph.Trace #-}
{-# LANGUAGE StandaloneDeriving, DerivingVia, TypeSynonymInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ProjectM36.Serialise.IsomorphicSchema where
import Codec.Winery
import Graph.Trace
import ProjectM36.IsomorphicSchema

deriving via WineryVariant SchemaExpr instance Serialise SchemaExpr
