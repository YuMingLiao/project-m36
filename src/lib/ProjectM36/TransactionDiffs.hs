{-# OPTIONS_GHC -fplugin=Graph.Trace #-}
module ProjectM36.TransactionDiffs where
import ProjectM36.Base
import Data.List.NonEmpty
import Graph.Trace
import Data.UUID as U

{-
root :: TransactionDiffs
root = single U.nil NoOperation

single :: TransactionId -> TransactionDiffExpr -> TransactionDiffs
single tid expr = (tid, expr) :| []
-}
