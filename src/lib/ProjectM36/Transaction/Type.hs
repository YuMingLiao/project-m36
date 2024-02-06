{-# OPTIONS_GHC -fplugin=Graph.Trace #-}
module ProjectM36.Transaction where
import Graph.Trace
import ProjectM36.Base

uid :: Transaction -> TransactionId
uid (Transaction uid _ _) = uid

info :: Transaction -> TransactionInfo
info (Transaction _ info _) = info
