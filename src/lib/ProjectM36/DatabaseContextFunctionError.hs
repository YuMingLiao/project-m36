{-# OPTIONS_GHC -fplugin=Graph.Trace #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module ProjectM36.DatabaseContextFunctionError where
import GHC.Generics
import Graph.Trace
import Control.DeepSeq

{-# ANN module ("HLint: ignore Use newtype instead of data" :: String) #-}
data DatabaseContextFunctionError = DatabaseContextFunctionUserError String
                                  deriving (Generic, Eq, Show, NFData)
